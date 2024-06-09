import Data.List
import Data.Char
import Data.Function

main :: IO ()
main = do
    print $ generate commands == Directory "/" [Directory "a"[Directory "e" [File "i" 584],File "f" 29116, File "g" 2557,File "h.lst" 62596], Directory "d" [File "d.ext" 5626152,File "d.log" 8033020,File "j" 4060174, File "k" 7214296],File "b.txt" 14848514,File "c.dat" 8504156]

type Command = String
type Size = Int
type Name = String

data FileSystem = Directory Name [FileSystem] | File Name Size
    deriving (Eq, Show)

generate :: [Command] -> FileSystem
generate commands = snd $ foldl' processCommand ([], Directory "/" []) commands
  where
    processCommand :: ([Name], FileSystem) -> Command -> ([Name], FileSystem)
    processCommand (path, fs) command
      | cmd == "$" && action == "cd" && target == ".." = (init path, fs)
      | cmd == "$" && action == "cd" = (path ++ [target], fs)
      | cmd == "$" && action == "ls" = (path, fs)
      | cmd == "dir" = (path, addDirectory path target fs)
      | otherwise = (path, addFile path target (read cmd) fs)
      where
        wordsCmd = words command
        cmd = head wordsCmd
        action = if length wordsCmd > 1 then wordsCmd !! 1 else ""
        target = if length wordsCmd > 2 then wordsCmd !! 2 else head (tail wordsCmd)

addDirectory :: [Name] -> Name -> FileSystem -> FileSystem
addDirectory [] _ fs = fs
addDirectory ["/"] name (Directory dirName children)
  | any (checkIsDirectory name) children = Directory dirName children
  | otherwise = Directory dirName (sortFileSystem (children ++ [Directory name []]))
addDirectory (p:ps) name (Directory dirName children)
  | p == dirName = Directory dirName (addOrUpdateDirectory children ps name)
  | otherwise = Directory dirName (map (addDirectory (p:ps) name) children)
addDirectory _ _ fs = fs

addOrUpdateDirectory :: [FileSystem] -> [Name] -> Name -> [FileSystem]
addOrUpdateDirectory children [] name
  | any (checkIsDirectory name) children = children
  | otherwise = sortFileSystem (children ++ [Directory name []])
addOrUpdateDirectory children (p:ps) name = map (addDirectory (p:ps) name) children

addFile :: [Name] -> Name -> Size -> FileSystem -> FileSystem
addFile [] _ _ fs = fs
addFile ["/"] name size (Directory dirName children)
  | any (checkIsFile name) children = Directory dirName children
  | otherwise = Directory dirName (sortFileSystem (children ++ [File name size]))
addFile (p:ps) name size (Directory dirName children)
  | p == dirName = Directory dirName (addOrUpdateFile children ps name size)
  | otherwise = Directory dirName (map (addFile (p:ps) name size) children)
addFile _ _ _ fs = fs

addOrUpdateFile :: [FileSystem] -> [Name] -> Name -> Size -> [FileSystem]
addOrUpdateFile children [] name size
  | any (checkIsFile name) children = children
  | otherwise = sortFileSystem (children ++ [File name size])
addOrUpdateFile children (p:ps) name size = map (addFile (p:ps) name size) children

sortFileSystem :: [FileSystem] -> [FileSystem]
sortFileSystem fs = sortDirs ++ sortFiles
  where
    sortDirs = sortOn (map toLower . getName) [d | d@(Directory _ _) <- fs]
    sortFiles = sortOn (map toLower . getName) [f | f@(File _ _) <- fs]
    getName (Directory name _) = name
    getName (File name _) = name

checkIsDirectory :: Name -> FileSystem -> Bool
checkIsDirectory name (Directory dirName _) = name == dirName
checkIsDirectory _ _ = False

checkIsFile :: Name -> FileSystem -> Bool
checkIsFile name (File fileName _) = name == fileName
checkIsFile _ _ = False

commands :: [Command]
commands = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]