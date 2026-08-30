module SyntaxTree.C
  ( C,
    Comment (..),
    CommentType (..),
    toLineComment,
    joinComments,
  )
where

import Data.Text qualified as T

data CommentType
  = InlineComment
  | LineComment
  deriving (Eq, Show, Ord)

data Comment = Comment
  { commentType :: CommentType,
    commentText :: T.Text
  }
  deriving (Eq, Show, Ord)

-- list of comments
type C = [Comment]

toLineComment :: Comment -> Comment
toLineComment c =
  c {commentType = LineComment}

joinComments :: C -> C -> C
joinComments c1 c2 =
  case c1 of
    [] ->
      c2
    _ ->
      c1 ++ map toLineComment c2
