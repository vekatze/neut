module Console.ReportMode
  ( ReportMode (..),
  )
where

data ReportMode
  = NoReport
  | PlainReport
  | FancyReport
  | DebugReport
  deriving (Eq)
