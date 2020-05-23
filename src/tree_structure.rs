use std::fmt;
use std::num::NonZeroUsize;
use std::ops::{Deref, DerefMut};

pub mod prelude {
    pub use super::{BranchBuilder, OwnedTree, Tree};
}

/// The maximum number of arguments a
/// tree node can have.
pub const MAX_ARGS: usize = 25;

struct Node<T> {
    /// The element itself.
    element: T,

    /// The length of the argument list
    /// after the element
    args_length: usize,

    /// Take the index of the node, subtract
    /// the parent index, and you get the index
    /// of the parent.
    ///
    /// In an ``OwnedTree``, this offset CANNOT
    /// point outside the tree.
    parent_offset: Option<NonZeroUsize>,

    n_args: usize,

    /// The offsets to the arguments after
    /// the first argument.
    /// The first argument is always directly
    /// after this node, so it doesn't really
    /// matter.
    args: [Option<NonZeroUsize>; MAX_ARGS],
}

impl<T: Clone> Clone for Node<T> {
    fn clone(&self) -> Self {
        Node {
            element: self.element.clone(),
            args_length: self.args_length,
            parent_offset: self.parent_offset,
            n_args: self.n_args,
            args: self.args.clone(),
        }
    }
}

/// An OwnedTree is a tree that is owned by
/// the user.
pub struct OwnedTree<T> {
    /// The length of the contents
    /// is always bigger than 0.
    contents: Vec<Node<T>>,
}

impl<T> OwnedTree<T> {
    /// Creates a new tree with a given first
    /// value.
    pub fn new(first_value: T) -> Self {
        OwnedTree {
            contents: vec![Node {
                element: first_value,
                args: [None; MAX_ARGS],
                args_length: 0,
                parent_offset: None,
                n_args: 0,
            }],
        }
    }

    pub fn build(&mut self) -> BranchBuilder<T> {
        BranchBuilder {
            owned: self,
            parent_index: 0,
        }
    }
}

impl<T> Deref for OwnedTree<T> {
    type Target = Tree<T>;

    fn deref(&self) -> &Self::Target {
        slice_into_tree(&self.contents)
    }
}

impl<T> DerefMut for OwnedTree<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        slice_into_tree_mut(&mut self.contents)
    }
}

pub struct BranchBuilder<'a, T> {
    owned: &'a mut OwnedTree<T>,
    parent_index: usize,
}

impl<'a, T> BranchBuilder<'a, T> {
    pub fn push_arg<'b>(
        &'b mut self,
        arg: T,
    ) -> BranchBuilder<'b, T> {
        let contents = &mut self.owned.contents;
        let new_index = contents.len();

        contents.push(Node {
            element: arg,
            args_length: 0,
            parent_offset: Some(
                NonZeroUsize::new(
                    new_index - self.parent_index,
                )
                .unwrap(),
            ),
            n_args: 0,
            args: [None; MAX_ARGS],
        });

        let parent_node = &mut contents[self.parent_index];
        parent_node.args[parent_node.n_args] = Some(
            NonZeroUsize::new(
                new_index - self.parent_index,
            )
            .unwrap(),
        );
        parent_node.n_args += 1;

        let mut parent_index = self.parent_index;
        loop {
            let mut parent = &mut contents[parent_index];
            parent.args_length += 1;

            match parent.parent_offset {
                Some(offset) => {
                    parent_index = parent_index
                        .checked_sub(offset.get())
                        .expect(
                            "Parent offset is out of range",
                        );
                }
                None => break,
            }
        }

        BranchBuilder {
            owned: &mut self.owned,
            parent_index: new_index,
        }
    }
}

impl<T> Deref for BranchBuilder<'_, T> {
    type Target = Tree<T>;

    fn deref(&self) -> &Self::Target {
        self.owned.branch(self.parent_index)
    }
}

impl<T> DerefMut for BranchBuilder<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.owned.branch_mut(self.parent_index)
    }
}

/// A simple tree. The length of the tree
/// _HAS_ to be at least 1.
pub struct Tree<T>([Node<T>]);

impl<T: Clone> Tree<T> {
    pub fn to_owned(&self) -> OwnedTree<T> {
        let mut contents = self.0.to_vec();

        // Keep the "parent_offset cannot point outside
        // the tree" invariant.
        for (i, node) in contents.iter_mut().enumerate() {
            if let Some(offset) = node.parent_offset {
                if i < offset.get() {
                    node.parent_offset = None;
                }
            }
        }

        OwnedTree { contents }
    }
}

impl<T> Tree<T> {
    /// Returns the head of the tree.
    pub fn get(&self) -> &T {
        &self.0[0].element
    }

    /// Returns the number of arguments that
    /// the head of the trees has.
    #[inline]
    pub fn n_args(&self) -> usize {
        self.0[0].n_args
    }

    pub fn branch(&self, loc: usize) -> &Tree<T> {
        let args = self.0[loc].args_length;
        slice_into_tree(&self.0[loc..=loc + args])
    }

    pub fn branch_mut(
        &mut self,
        loc: usize,
    ) -> &mut Tree<T> {
        let args = self.0[loc].args_length;
        slice_into_tree_mut(&mut self.0[loc..=loc + args])
    }

    pub fn args<'a>(&'a self) -> Args<'a, T> {
        Args {
            tree: self,
            current: 1,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Tree<T> {
    fn fmt(
        &self,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        write!(f, "{:?}", self.get())?;
        if self.n_args() > 0 {
            write!(f, " ")?;
            let mut set = f.debug_set();
            for arg in self.args() {
                set.entry(&arg);
            }
            set.finish();
        }
        Ok(())
    }
}

/// An iterator over the arguments of a tree
/// node.
pub struct Args<'a, T> {
    tree: &'a Tree<T>,
    current: usize,
}

impl<'a, T> Iterator for Args<'a, T> {
    type Item = &'a Tree<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current < self.tree.0.len() {
            let args =
                self.tree.0[self.current].args_length;
            let pos = self.current;
            self.current += args + 1;
            Some(self.tree.branch(pos))
        } else {
            None
        }
    }
}

fn slice_into_tree<'a, T>(
    slice: &'a [Node<T>],
) -> &'a Tree<T> {
    assert!(slice.len() > 0);
    use std::mem::transmute;
    // SAFETY: This should be safe, because
    // the memory representation of a Tree and a slice
    // are exactly the same. Not sure if that is perfectly
    // defined though, so I'm a little bit scared to do this.
    unsafe { transmute(slice) }
}

fn slice_into_tree_mut<'a, T>(
    slice: &'a mut [Node<T>],
) -> &'a mut Tree<T> {
    assert!(slice.len() > 0);
    use std::mem::transmute;
    // SAFETY: This should be safe, because
    // the memory representation of a Tree and a slice
    // are exactly the same.
    unsafe { transmute(slice) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::drop;

    #[derive(Debug, PartialEq)]
    enum TestNode {
        Section,
        Number(u64),
        Paragraph(&'static str),
    }

    #[test]
    fn to_owned() {
        let mut tree = OwnedTree::new(TestNode::Section);
        let mut builder = tree.build();
        builder.push_arg(TestNode::Number(1));
        {
            let mut builder =
                builder.push_arg(TestNode::Section);
            builder.push_arg(TestNode::Number(5));
            builder.push_arg(TestNode::Number(7));
        }
        builder.push_arg(TestNode::Number(3));

        let section =
            tree.args().skip(1).next().unwrap().to_owned();
        assert_eq!(section.get(), &TestNode::Section);
        assert_eq!(section.n_args(), 2);
    }

    #[test]
    fn build_massive() {
        let mut tree = OwnedTree::new(TestNode::Number(1));
        let mut args = tree.build();

        fn build_more(
            mut args: BranchBuilder<'_, TestNode>,
            n: u64,
        ) {
            if n > 100 {
                return;
            }

            args.push_arg(TestNode::Paragraph(
                "Hello world!",
            ));
            let new_args = args.push_arg(TestNode::Section);
            build_more(new_args, n + 1);
            args.push_arg(TestNode::Number(n));
        }

        build_more(args, 0);
    }

    #[test]
    fn building_tree() {
        let mut tree = OwnedTree::new(TestNode::Section);
        let mut args = tree.build();

        args.push_arg(TestNode::Number(5));
        args.push_arg(TestNode::Paragraph("Hello"));
        let mut section_args =
            args.push_arg(TestNode::Section);
        section_args.push_arg(TestNode::Number(2));
        section_args.push_arg(TestNode::Number(6));
        drop(section_args);
        drop(args);

        let mut args = tree.args();
        let next = args.next().unwrap();
        assert_eq!(next.get(), &TestNode::Number(5));
        assert_eq!(
            next.args().next().map(|v| v.get()),
            None
        );

        let next = args.next().unwrap();
        assert_eq!(
            next.get(),
            &TestNode::Paragraph("Hello")
        );
        assert_eq!(
            next.args().next().map(|v| v.get()),
            None
        );

        let next = args.next().unwrap();
        let mut next_args = next.args();
        assert_eq!(next.get(), &TestNode::Section);
        assert_eq!(
            next_args.next().map(|v| v.get()),
            Some(&TestNode::Number(2))
        );
        assert_eq!(
            next_args.next().map(|v| v.get()),
            Some(&TestNode::Number(6))
        );
        assert_eq!(next_args.next().map(|v| v.get()), None);
    }
}
