module Main where

import Conduit
import Control.Monad
  ( forever,
    void,
    when,
  )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Discord.Voice
import Discord.Voice.Conduit
import Options.Applicative
import qualified StmContainers.Map as M
import UnliftIO
  ( atomically,
  )

data BotAction
  = PlayVoice String
  | LeaveVoice
  | DisplayQueue

data GuildContext = GuildContext
  { songQueries :: [String],
    leaveFunc :: Voice () -- function to leave voice channel
  }

parser :: ParserInfo BotAction
parser = flip info fullDesc $ helper <*> commandParser
  where
    commandParser =
      subparser $
        mconcat
          [ playParser,
            leaveParser,
            displayQueueParser
          ]

    playParser =
      command "play" $
        flip info (progDesc "Queue something to play!") $
          PlayVoice . unwords
            <$> some (argument str (metavar "QUERY" <> help "Search query/URL"))
    leaveParser =
      command "leave" $
        info (pure LeaveVoice) (progDesc "Leave a voice channel")
    displayQueueParser =
      command "queue" $
        info (pure DisplayQueue) (progDesc "Display the queue")

main :: IO ()
main = do
  tok <- TIO.readFile "./auth-token.secret"

  queries <- M.newIO
  userFacingError <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnStart = pure (),
          discordOnEnd = liftIO $ putStrLn "Ended",
          discordOnEvent = eventHandler queries,
          discordOnLog = TIO.putStrLn
        }
  TIO.putStrLn userFacingError
  TIO.putStrLn "Exiting..."

eventHandler :: M.Map String GuildContext -> Event -> DiscordHandler ()
eventHandler contexts (MessageCreate msg) = case messageGuildId msg of
  Nothing -> pure ()
  Just gid -> do
    let args = map T.unpack $ T.words $ messageContent msg
    case args of
      ("bot" : _) -> case execParserPure defaultPrefs parser $ tail args of
        Success x -> handleCommand contexts msg gid x
        Failure failure ->
          void $
            restCall $
              R.CreateMessage (messageChannelId msg) $
                T.pack $
                  fst $
                    renderFailure failure "bot"
        _ -> pure ()
      _ -> pure ()
eventHandler _ _ = pure ()

playYouTube :: String -> Voice ()
playYouTube query = do
  let adjustVolume = awaitForever $ \current -> yield current
  resource <- createYoutubeResource query $ Just $ HaskellTransformation $ packInt16C .| adjustVolume .| unpackInt16C

  case resource of
    Nothing -> liftIO $ print "whoops"
    Just re -> play re UnknownCodec

joinVoiceChannel :: M.Map String GuildContext -> GuildId -> String -> DiscordHandler ()
joinVoiceChannel contexts gid query = do
  response <- restCall $ R.GetGuildChannels gid
  mbCid <- case response of
    Left err -> liftIO (print err) >> return Nothing
    Right channels -> return $ (Just . channelId . last) [c | c@ChannelVoice {} <- channels]
  result <- runVoice $ case mbCid of
    Nothing -> return ()
    Just cid -> do
      leave <- join gid cid
      liftDiscord $ atomically $ M.insert (GuildContext [query] leave) (show gid) contexts
      -- Forever, read the top of the queue and play it
      forever $ do
        context <- liftDiscord $ atomically $ M.lookup (show gid) contexts
        case context of
          Nothing -> pure ()
          Just (GuildContext [] _) -> pure ()
          Just (GuildContext (x : xs) _) -> do
            liftDiscord $ atomically $ M.insert (GuildContext xs leave) (show gid) contexts
            playYouTube x
  case result of
    Left e -> void $ liftIO (print e)
    Right _ -> return ()

handleCommand :: M.Map String GuildContext -> Message -> GuildId -> BotAction -> DiscordHandler ()
handleCommand contexts _msg gid LeaveVoice = do
  context <- atomically $ M.lookup (show gid) contexts
  case context of
    Nothing -> pure ()
    Just (GuildContext _ leave) -> do
      void $ atomically $ M.delete (show gid) contexts
      void $ runVoice leave
handleCommand contexts msg gid (PlayVoice q) = do
  resultQueue <- atomically $ do
    context <- M.lookup (show gid) contexts
    case context of
      Nothing -> return []
      Just (GuildContext xs leave) -> do
        M.insert (GuildContext (xs ++ [q]) leave) (show gid) contexts
        return $ xs ++ [q]
  when (null resultQueue) (joinVoiceChannel contexts gid q)
  void $
    restCall $
      R.CreateMessage (messageChannelId msg) $
        T.pack $
          "Queued for playback: " <> show q
handleCommand contexts msg gid DisplayQueue = do
  context <- atomically $ M.lookup (show gid) contexts
  let queue = case context of
        Nothing -> []
        Just c -> songQueries c
  void $
    restCall $
      R.CreateMessage (messageChannelId msg) $
        T.pack $
          queueToString queue

-- Helpers
queueToString :: [String] -> String
queueToString [] = "Empty queue"
queueToString xs =
  concatMap
    (\(i, x) -> show i <> ". " <> x <> "\n")
    (zip ([1 ..] :: [Int]) xs)
