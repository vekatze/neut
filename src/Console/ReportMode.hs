module Console.ReportMode
  ( ReportMode (..),
  )
where

data ReportMode
  = AutoReport
  | NoReport
  | PlainReport
  | FancyReport
  | DebugReport
  deriving (Eq)
