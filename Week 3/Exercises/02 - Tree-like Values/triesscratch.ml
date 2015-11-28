update_children

let samp = [('p', Trie (None, [('m', Trie (None, [('d', Trie (Some 3, []))]))]));
            ('d', Trie (None, [('m', Trie (Some (-1), []))]));
            ('j', Trie (Some 2, [('g', Trie (None, [('g', Trie (Some 1, []))]))]))];;

  'j'

let rep = (Trie (Some 2,
                 [('j', Trie (None, [('a', Trie (None, [('g', Trie (Some 0, []))]))]));
                  ('p',
                   Trie (None,
                         [('j', Trie (None, [('d', Trie (Some (-4), []))]));
                          ('g', Trie (Some 0, []))]));
                  ('a', Trie (None, [('p', Trie (Some 0, []))]))]));;
