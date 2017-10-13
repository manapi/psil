-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un numbre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Scons Snil (Ssym "quote")) e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do pChar '('
           pSpaces
           (do { pChar ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do pChar '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do pChar ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where
      showHead (Scons Snil e2') = showString "(" . showSexp' e2'
      showHead (Scons e1' e2') = showHead e1' . showString " " . showSexp' e2'
      showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String
type Tag = String
type Pat = Maybe (Tag, [Var])
data BindingType = Lexical | Dynamic
                   deriving (Show, Eq)
    
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Llambda [Var] Lexp  -- Fonction anonyme prenant un argument.
          | Lapp Lexp [Lexp]    -- Appel de fonction, avec un argument.
          | Lcons Tag [Lexp]    -- Constructeur de liste vide.
          | Lcase Lexp [(Pat, Lexp)] -- Expression conditionelle.
          | Llet BindingType Var Lexp Lexp -- Déclaration de variable locale
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
-- ¡¡ COMPLETER !!
s2l (Scons Snil e) = s2l e
s2l (Scons e1 e2) = 
    let e1' = s2l e1
        e2' = s2l e2
    in case (e1', e2') of
	
	 (Lcons tag tab, _ ) -> Lcons tag (tab++[e2']) 
	 (Lapp (Lcase exp cas) (a:as), _ ) ->  Lcase exp [(Just("true",[]),a),(Just("false",[]),e2')]
	 
	 (Lapp e arg, Lnum n) -> Lapp e (arg++[e2'])
	 
	 
	 ( _ , Lnum n) -> Lapp e1' [e2']
	 
	 (Lvar "if", _ ) -> Lcase e2' []
	 (Lvar "case", _ ) -> Lcase e2' []
	 
	 (Lvar "cons", Lvar "cons") -> Lcons "cons" []
	 (Lvar "cons", Lvar "nil") -> Lcons "nil" []
	 (Lvar "cons", Lvar v ) -> Llambda ["cons", v] (Lvar "//CASE//")
	
	 (Lvar s, Lapp e arg) -> Lapp e1' [e2']
	 (Lapp e arg, Lvar n) -> Lapp e (arg++[e2'])
	 
	 (Lvar "lambda", Lvar v) -> Llambda [v] (Lvar " ")
	 (Llambda var1 (Lvar " "), Llambda var2 exp2) -> Llambda (var1++var2) exp2
	 (Llambda var (Lvar "//CASE//"), Lvar v) -> Llambda (var++[v]) (Lvar "//CASE")
	 (Llambda (x:xs) (Lvar "//CASE//"), Lapp _ _ ) -> Lcase (Lvar " ") [(Just(x, xs), e2')]
	 (Llambda var (Lvar " "), _ ) -> Llambda var e2' 
	 
	 (Lcase e cas, Lapp (Lvar v) (a:as)) -> Lcase e (cas++[(Just (v,[]),a)])
	 (Lcase e cas1, Lcase (Lvar " ") cas2) -> Lcase e (cas1++cas2)
	 
	 (_ , Lvar v) -> Lapp e1' [e2']

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

type Arity = Int

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vcons Tag [Value]
           | Vfun Arity (Env -> [Value] -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vcons tag vs) =
        let showTail [] = showChar ']'
            showTail (v : vs') =
                showChar ' ' . showsPrec p v . showTail vs'
        in showChar '[' . showString tag . showTail vs
    showsPrec _ (Vfun arity _)
        = showString ("<" ++ show arity ++ "-args-function>")

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let false = Vcons "false" []
           true = Vcons "true" []
           mkbop (name, op) =
               (name, Vfun 2 (\ _ [Vnum x, Vnum y] -> Vnum (x `op` y)))
           mkcmp (name, op) =
               (name, Vfun 2 (\ _ [Vnum x, Vnum y]
                                  -> if x `op` y then true else false))
       in [("false", false),
           ("true", true)]
          ++ map mkbop
              [("+", (+)),
               ("*", (*)),
               ("/", div),
               ("-", (-))]
          ++ map mkcmp
              [("<=", (<=)),
               ("<", (<)),
               (">=", (>=)),
               (">", (>)),
               ("=", (==))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

eval :: Env -> Env -> Lexp -> Value
eval _senv _denv (Lnum n) = Vnum n
-- ¡¡ COMPLETER !!
eval _senv _denv (Lvar x) = envlookup x _senv -- handle dynamic lookup too / look into both env 
eval _senv _denv (Lapp f args) = 
    let f' = eval _senv _denv f
        g = eval _senv _denv
        args' = map g args
	in case (f', args') of
	    (Vfun a f, _) -> f _senv args'
eval _ _ e = error ("Can't eval: " ++ show e)


-- Fonctions auxiliaires

-- Environnement lookup
envlookup :: Var -> Env -> Value
envlookup x ((x',v'):rest) = if x == x' then v' else envlookup x rest

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 [] . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))

-- To remove
main = run "temptest.psil"