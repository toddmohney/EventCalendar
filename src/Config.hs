module Config where
  import Control.Applicative
  import System.Environment (getEnv)

  type ConfigPair = (String, IO String)

  postgresConfig :: [ConfigPair]
  postgresConfig = [ 
                     ("host",    getEnv "EVENT_CALENDAR_DB_HOST")
                   , ("dbname",  getEnv "EVENT_CALENDAR_DB_NAME")
                   , ("user",    getEnv "EVENT_CALENDAR_DB_USER")
                   , ("port",    getEnv "EVENT_CALENDAR_DB_PORT")
                   ]

  mergePairs :: Char -> [ConfigPair] -> [IO String]
  mergePairs sep [] = []
  mergePairs sep (x:xs) = (mergePair sep x):(mergePairs sep xs)

  mergePair :: Char -> ConfigPair -> IO String
  mergePair sep pair = (((fst pair) ++ [sep]) ++) <$> (snd pair)

  getPostgresConnectionString :: IO String
  getPostgresConnectionString = foldr1 concatWithSpaceSeparator $ mergePairs '=' postgresConfig
    where 
      concatWithSpaceSeparator :: Applicative f => f String -> f String -> f String
      concatWithSpaceSeparator list = (\x acc -> (++) <$> (++ " ") <$> x <*> acc) list
