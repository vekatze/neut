module SyntaxTree.C (C, Comment (..), CommentType (..)) where

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
