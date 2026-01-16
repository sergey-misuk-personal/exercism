module BinarySearchTree exposing (BinaryTree(..), makeTree, sort)


type BinaryTree
    = Empty
    | Tree BinaryTree Int BinaryTree


makeTree : List Int -> BinaryTree
makeTree data =
    let
        insert : Int -> BinaryTree -> BinaryTree
        insert value tree =
            case tree of
                Empty ->
                    Tree Empty value Empty

                Tree l v r ->
                    case compare value v of
                        GT ->
                            Tree l v (insert value r)

                        _ ->
                            Tree (insert value l) v r
    in
    List.foldl insert Empty data


sort : List Int -> List Int
sort data =
    let
        makeList : BinaryTree -> List Int
        makeList tree =
            case tree of
                Empty ->
                    []

                Tree l v r ->
                    makeList l ++ [ v ] ++ makeList r
    in
    data
        |> makeTree
        |> makeList
