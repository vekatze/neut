module SyntaxTree.C
  ( C,
    Comment (..),
    CommentType (..),
    toLineComment,
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
