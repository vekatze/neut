module Console.FormatMode
  ( FormatMode (..),
  )
where

data FormatMode
  = Check
  | Stdout
  | Write
  deriving (Eq)
