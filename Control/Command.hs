module Control.Command
(
-- * Command Type
  CommandT
, runCommandT
--, Command
-- * Basic Parsers
, word
, match
, matchCI
, rest
, end
-- * Select
, Select
, select
, select'
, parseSelect 
--, parseSelectDef
, promptSelect 
-- ** Basic Selects
, selYesNo
, cancellable
)
where


import Control.Arrow ( second )
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Monoid
import Data.Function ( on )
import Data.List ( intercalate )

import qualified Text.Parsec as P
import Text.Parsec ( manyTill, anyChar, string, eof, spaces, choice, (<?>))

import qualified Data.CaseInsensitive as CI ( mk )


-- | Perform a case-insensitive String comparison
compareCI :: String -> String -> Bool
compareCI = (==) `on` CI.mk

guarding :: (MonadPlus m) => (a -> Bool) -> a -> m a
guarding p a = guard (p a) >> return a



type CommandT = P.ParsecT String ()

--type Command = CommandT 


runCommandT :: (Monad m) => CommandT m a -> String -> m (Maybe a)
runCommandT m = liftM hush . P.runPT m () ""
  where
    hush = either (const Nothing) Just



-- | Parses a string of continuous non-space characters, and consumes all
-- spaces that immediately follow it.
--
-- TODO: verify the following property:
--
-- @
-- 'P.many' word === fmap words . many 'anyChar'
-- @
--
word :: (Monad m) => CommandT m String
word = manyTill anyChar (eof <|> spaces) <?> "word"

rest :: (Monad m) => CommandT m String
rest = manyTill anyChar eof <?> "rest"

-- | @match s@ is a parser that only succeeds when input matches @s@ exactly.
match :: (Monad m) => String -> CommandT m String
match s = string s <* end <?> "match " ++ show s

-- | A case-insensitive version of @match@.
matchCI :: (Monad m) => String -> CommandT m String
matchCI s = test (compareCI s)

-- | @test p@ is a parser that succeeds on input @s@ iff @p s@.
test :: (Monad m) => (String -> Bool) -> CommandT m String
test p = guarding p =<< rest

-- | Parser that succeeds at end of line. A specialized version of
-- 'P.eof'.
end :: (Monad m) => CommandT m ()
end = eof


data Name = Name
    { name    :: String
    , aliases :: [String]
    } deriving (Show, Eq, Ord)

names :: Name -> [String]
names = (:) <$> name <*> aliases

-- | Perform a case-insensitive comparison between a 'String' and a 'Name'.
compareName :: String -> Name -> Bool
compareName s = any (compareCI s) . names

-- | @matchName n@ is a parser that succeeds on an input @s@ iff
-- @compareName s n@.
matchName :: (Monad m) => Name -> CommandT m String
matchName = test . flip compareName


-- | @Select a@ represents a list of options that may be input at a prompt,
-- each corresponding to a value of type @a@.
newtype Select a = Sel { unSel :: [(Name, a)] }

instance Functor Select where
    fmap f = Sel . map (second f) . unSel

instance Monoid (Select a) where
    mempty = Sel []
    Sel xs `mappend` Sel ys = Sel (xs ++ ys)


-- | @select name result@ will create a @Select@ value containing the
-- single option @name@, and a parser that matches @name@ and returns
-- @result@.
select :: String -> a -> Select a
select name = select' name []

-- | @select' name aliases result@ performs the function of @select name
-- result@ but the created parser will also succeed on any element of
-- @aliases@.
select' :: String -> [String] -> a -> Select a
select' name aliases result = Sel [(Name name aliases, result)]

selectNames :: Select a -> [String]
selectNames = map (name . fst) . unSel

-- | Format a concise String representation of available options.
showOptions :: Select a -> String
showOptions = ("(" ++) . (++ ")") . intercalate "|" . selectNames

-- | Compute a parser for a 'Select'.
parseSelect :: (Monad m) => Select a -> CommandT m a
parseSelect = choice . map selParse . unSel
  where
    selParse (n, v) = v <$ matchName n

{-
-- | @parseSelectDef d xs@ computes a parser for the Select @d@ and list of
-- Selects @s@. If the user inputs a blank line, @d@ is used as the
-- default value.
parseSelectDef :: (Monad m)
               => Select a    -- ^ Default selection value
               -> [Select a]  -- ^ Additional choices
               -> CommandT m a
parseSelectDef d ss = defParse d
                  <|> parseSelect (d : ss)
                  <?> showOptions ss
  where
    defParse d = selValue d <$ end
-}

-- A monadic function from a prompt to user input
type UserInput m = String -> m String

-- | Prompt the user for a selection.
promptSelect :: (Monad m) => UserInput m -> Maybe a -> Select a -> m (Maybe a)
promptSelect input mdefault s = 
    case unSel s of
        -- no options
        []      -> return Nothing
        -- single option
        [(n,v)] -> return (Just v)
        -- multiple options - prompt the user
        _ -> runCommandT parser =<< input (showOptions s)
  where
    parser = def <|> parseSelect s
    def = case mdefault of
            Nothing -> mzero
            Just v  -> v <$ end

selYes :: Select Bool
selYes = select' "y" ["yes"] True

selNo :: Select Bool
selNo = select' "n" ["no"] False

-- | A simple Select that prompts the user for \"yes\" or \"no\".
selYesNo :: Select Bool
selYesNo = select' "y" ["yes"] True
        <> select' "n" ["no"]  False

cancellable :: Select a -> Select (Maybe a)
cancellable s = select' "c" ["cancel"] Nothing
             <> fmap Just s
