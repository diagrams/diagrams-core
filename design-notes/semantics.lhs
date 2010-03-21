\documentclass{article}

%include polycode.fmt

\usepackage{amsmath}

\newcommand{\A}{\ensuremath{\mathbb{A}}}
\newcommand{\N}{\ensuremath{\mathbb{N}}}
\newcommand{\R}{\ensuremath{\mathbb{R}}}

\begin{document}

%if false
\begin{code}
{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances,
             TypeSynonymInstances #-}

import Data.Monoid
import Data.List
import Control.Arrow
import qualified Data.Map as M

type AName = String
type Name = [AName]
\end{code}
%endif

%format AName        = "\A"
%format Name         = "\N"
%format Double       = "\R"

%format M.Map a b    = a "\mapsto" b
%format M.union      = "(\cup)"
%format `M.union`    = "\cup"
%format M.mapKeys    = "mapKeys"
%format M.map        = "mapValues"
%format M.insert k v = "[" k "\mapsto" v "]"

%format mempty    = "\varepsilon"
%format `mappend` = "\bullet"

%format ***  = "\times"

This file documents the current understanding of the \emph{semantics}
for diagrams.  Note that this does not necessarily mean that the
diagrams library will \emph{implement} things in exactly this way, but
it does mean that the implementation should respect and preserve these
semantics, and that it should be possible to write documentation which
uses these semantics to give intuition to users of the library.

This document is literate Haskell, providing a simple (but possibly
inefficient) executable model of the semantics.

\section{Primitives and transformations}
\label{sec:prims}

We take as given a set of ``primitives'' which can be rendered by
various rendering backends.  The overall goal of the diagrams library
is to provide a DSL for specifying a set of primitives which should be
rendered in a certain way.  The semantics of primitives themselves is
not the concern of this document; we consider them to be (mostly)
opaque.

We also have a monoid of ``transformations'' |t|.  The intuition
is that |t| represents transformations on the space in which the
primitives are being laid out.  We also have a class |Transformable|
representing things to which transformations can be applied:

\begin{code}
class Monoid t => Transformable t a where
  transform :: t -> a -> a  
\end{code}

The intention is that primitives should be |Transformable|, but as we
will see, other things will be |Transformable| as well.

We require that |transform| is a monoid action, that is, 
\begin{enumerate}
\item |transform mempty a == a|, and
\item |transform (t1 `mappend` t2) a == transform t1 (transform t2 a)|
\end{enumerate}
where |mempty| and |`mappend`| denote the monoid unit and binary
operation, respectively.

\section{Layouts}
\label{sec:layouts}

Semantically, a |Layout| is just a list (of primitives):

\begin{code}
type Layout a = [a]
\end{code}

Two |Layout|s can be composed by placing one |over| another, the idea
being that in |l1 `over` l2|, the primitives in |l1| will be rendered
over/in front of the primitives in |l2|.  Semantically, this just
corresponds to list concatenation:

\begin{code}
over :: Layout a -> Layout a -> Layout a
over = (++)
\end{code}

|transform| also lifts to |Layout|s in a natural way; |transform t l|
applies |transform t| to each of the individual primitives in the
layout |l|.  In fact, |transform| automatically lifts to any functor
applied to |Transformable|s:

\begin{code}
instance (Functor f, Transformable t a) => Transformable t (f a) where
  transform = fmap . transform
\end{code}

\section{Names}
\label{sec:names-and-constraints}

When composing layouts we may wish to position them relative to one
another using constraints; in order to write the constraints we need
names to refer to various parts of the layouts being composed.

We take as given a set of ``atomic names'' |AName| with decidable
equality, and say that a ``name'' is a nonempty sequence of atomic
names.  We abbreviate the set of names as |Name|.

A |NameSet| is a finite map from names to transformations, which
supports two primitive operations (in addition to standard operations
on finite maps, such as constructing the empty or singleton maps,
insertion, (left-biased) union, |mapKeys|, |mapValues|, \emph{etc.}):
|qualify| and |rememberAs|.

\begin{code}
type NameSet t = M.Map Name t
\end{code}

Qualifying a |NameSet| prefixes an atomic name to the beginning of
every name in the mapping.  This is just like doing a ``qualified
import'' in a module system.

\begin{code}
qualify :: AName -> NameSet t -> NameSet t
qualify n = M.mapKeys (n:)
\end{code}

|rememberAs| gives a name to the current identity transformation:

\begin{code}
rememberAs :: Monoid t => Name -> NameSet t -> NameSet t
rememberAs n = M.insert n mempty
\end{code}

A previous version of this document suggested defining a derived
operation which performed both |qualify| and |rememberAs|. However,
this isn't a good idea, since it leads to unintuitive behavior.  Upon
reflection this is not unexpected: |qualify| and |rememberAs| have
quite different purposes; conflating them only leads to confusion.

We can also transform |NameSet|s, by combining the given
transformation with every transformation in the map.

\begin{code}
instance (Monoid t) => Transformable t (NameSet t) where
  transform = M.map . mappend
\end{code}

\section{Combining Names and Layouts}
\label{sec:combining}

A |NamedLayout| is a |Layout| paired with a |NameSet|.

\begin{code}
type NamedLayout t a = (Layout a, NameSet t)
\end{code}

The intuition here is that while building a layout, we may have wanted
to ``remember'' some of the ``local coordinate systems'' (\emph{i.e.}
transformations) corresponding to sublayouts, by giving them names.

We can lift |transform| and |over| to act on |NamedLayouts| in the
obvious way:

\begin{code}
instance (Transformable t a) => Transformable t (NamedLayout t a) where
  transform t = transform t *** transform t
\end{code}

|NamedLayout|s have an automatically derived |Monoid| instance which
corresponds to putting the first layout atop the second, and taking
the (left-biased) union of the |NameSet|s.

\section{Constraints}
\label{sec:constraints}

% Now, we had been thinking along the lines of 

%   composeWithConstraints :: [Diagram] -> [Constraint] -> Diagram

% but I think we can simplify this along the same lines as you
% simplified the 'compose' operation above; the above
% composeWithConstraints could be implemented in terms of simpler
% primitive operations.  In particular, consider the case of composing
% two layouts with a constraint.  Now, there are actually TWO degrees of
% freedom when composing two layouts: the offset between the two
% layouts, and the offset from the origin of the new local coordinate
% system.  However, we don't lose anything if we simply specify that the
% local coordinate system of the composed layout is the same as that of
% the first.

%   composeConstrained :: NamedLayout t a -> NamedLayout t a -> Constraint -> NamedLayout t a

% A 'Constraint' is an equation of type t that is allowed to mention
% precisely one name from the first NamedLayout, and one name from the
% second.  Suppose that it mentions n1 and n2, which are paired with
% transformations t1 and t2 in the first and second NamedLayouts
% respectively.  Then we substitute t1 for n1 (this corresponds to
% requiring that the local coordinate system of the resulting diagram is
% the same as that of the first), and solve the equation for n2,
% resulting in a transformation t2' which specifies where n2 ought to
% end up in the local coordinate system of the first layout.  To achieve
% %this, we have to apply t2' . t2^{-1} to the second layout.  That is,

% %  composeConstrained nl1 nl2 = nl1 `under` transform' (t2' . t2^{-1}) nl2

% Hmm... this still seems more complicated than necessary to me, but I
% think it is heading in the right direction.  Maybe someone else has a
% good idea of how to simplify it.  I guess it's also worth thinking
% more about exactly what a 'Constraint' is and what sort of operations
% are supported on it.

\section{Regions}
\label{sec:regions}

Every diagram may optionally have an associated \emph{bounding
  region}, which is used to help automatically position diagrams
relative to one another. 

Semantically, a |Region| is just a subset of $\R^2$:

\begin{code}
type Region = Double -> Double -> Bool
\end{code}

|Bool| forms a Boolean algebra, and hence so does |Region|, where we
lift | |||| |, |&&|, and $\neg$ pointwise.  In other words, regions
support join (union), meet (intersection) and complement operations,
with the empty region $\varnothing$ and universal region $\R^2$ serving
as $\bot$ and $\top$ respectively.  We are especially interested in
the monoid $(\cup, \varnothing)$ on |Region|s, although the other
operations may be useful as well.

We can associate a |Region| with every diagram to serve as a
``bounding region'' (diagrams which do not have a natural ``bounding
region'' may simply be assigned the universal region $\R^2$).  We then
get a naturally derived monoid on |(Diagram, Region)| pairs which
takes the union of diagrams' bounding regions while combining the
diagrams.  The bounding region of a diagram may be used to
automatically position two diagrams adjacent to one another, without
the user having to remember, or even know, how large the diagrams are.

Of course, |Double -> Double -> Bool| is quite an inefficient
\emph{implementation} of regions.  The challenge will be to come up
with a suitable induced Boolean subalgebra which is computationally
tractable, yet sufficiently expressive.  For example, we might
consider only collections of polygonally-bounded regions, which are
clearly closed under union, intersection, and complementation.
However, this might not be sufficiently expressive; we might want to
be able to have bounding regions with curved boundaries.

\section{Paths}
\label{sec:paths}




\end{document}
