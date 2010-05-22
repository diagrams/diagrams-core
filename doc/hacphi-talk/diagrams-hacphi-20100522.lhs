%% -*- LaTeX -*-
\documentclass[xcolor=svgnames]{beamer}

\usepackage{brent}

%include polycode.fmt

\renewcommand{\onelinecomment}{--- \itshape}
\renewcommand{\Conid}{\mathsf}
\renewcommand{\Varid}{\mathsf}

\mode<presentation>
{
  \usetheme{default}                          % use a default (plain) theme
  \setbeamertemplate{navigation symbols}{}    % don't show navigation
                                              % buttons along the
                                              % bottom

  \setbeamertemplate{footline}[frame number]

  \AtBeginSection[]
  {
    \begin{frame}<beamer>
      \frametitle{Outline}
      \tableofcontents[currentsection,currentsubsection]
    \end{frame}
  }
}

\usepackage[english]{babel}
\usepackage{graphicx}
\usepackage{ulem}
\usepackage{url}

\newif \iftext \texttrue

\newcommand{\stext}[1]{\iftext \begin{center}#1\end{center} \fi}

\title{Declarative, embedded drawing with \texttt{diagrams}: past and future}
\date{Hac $\varphi$ \\ May 21, 2010}
\author{Brent Yorgey \\ University of Pennsylvania}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

\begin{frame}{}
  \titlepage
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=2in]{images/man-itch.jpg}
  \end{center}
  \stext{The diagrams library started to scratch a personal itch.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=4in]{images/code-to-dia.pdf}
  \end{center}
  \stext{The goal: programmatically generate drawings and diagrams in
    a way that is declarative, powerful, and semantically elegant.}
\end{frame}

\begin{frame}{}
  \vspace{0.3in}
  \begin{center}
    \includegraphics[width=1.5in]{images/MPlogo.png}
  \end{center}
  \stext{I first looked into existing solutions.  The most obvious
    candidate is MetaPost.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=1.75in]{images/MPlogo-no.png}
  \end{center}
  \stext{But it's not sufficiently declarative, and uses a weird
    ad-hoc language which isn't general-purpose.}
\end{frame}

\begin{frame}{}
  \vspace{0.1in}
  \begin{center}
    \includegraphics[width=1.5in]{images/Asymptote-logo.png}
  \end{center}
  \stext{What about Asymptote, which is supposed to be a modern
    replacement for MetaPost?}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=1.72in]{images/Asymptote-logo-no.png}
  \end{center}
  \stext{No thanks: at least it uses a general-purpose language (so we
    can compute the diagrams we want to describe), but it's a TERRIBLE
    language combining the worst features of C++ and Java.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=1.7in]{images/pgf-tikz.pdf}
  \end{center}
  \stext{What about PGF/TikZ?}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=1.7in]{images/pgf-tikz-no.pdf}
  \end{center}
  \stext{That's right, it uses an ad-hoc, non-general purpose language
    and\dots sigh.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=4in]{images/diagrams.pdf}
  \end{center}
  \stext{Thus, the \texttt{diagrams} library was born!  It's gotten a
    bit of use, people seem to like it. It got quite a few things
    right, but let's look at some things it got wrong.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/one.pdf}
  \end{center}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/ab.pdf}
  \end{center}
  \stext{A fundamental ability of the library is to put two diagrams
    next to each other to create a larger diagram.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/ab-boxed.pdf}
  \end{center}
  \stext{An obvious way to accomplish this is with bounding boxes.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/ab-boxed-rot.pdf}
  \end{center}
  \stext{What happens when we want to rotate the triangle? The above
    is what the currently released version of \texttt{diagrams} does
    --- I've gotten bug reports about it, and I agree it's a bug.  But
  it's not clear what the real solution is.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/ab-boxed-rot-adj.pdf}
  \end{center}
  \stext{Why not just adjust the box?  Not so fast---this requires
    knowing more about the diagram than just its bounding box in the
    first place!}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/ab-boxed-rotbox.pdf}
  \end{center}
  \stext{We could do this, but now transformations don't
    compose---rotating by A then by B gives a different bounding box
    than just rotating by A+B.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/ab-boxed-diag.pdf}
  \end{center}
  \stext{And what about putting things next to each other along a line
    that isn't vertical or horizontal?  We get this\dots}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/ab-boxed-diag-adj.pdf}
  \end{center}
  \stext{\dots instead of this.}  
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/triangle-bounds.pdf}
  \end{center}
  \stext{An elegant solution, suggested by Sebastian Setzer, is to have a
    function giving the distance to the nearest enclosing (hyper)plane
    in a given direction (relative to some distinguished base point).
    In some sense this gives us a functional representation of a
    convex bounding region.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/triangle-bounds-rot.pdf}
  \end{center}
  \stext{This obviously works beautifully with rotation! In fact, it
    works with any affine transformation.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/triangle-circle-bounds.pdf}
  \end{center}
  \stext{We can put two diagrams next to each other along any vector
    by putting them alongside a separating plane.  It's not ``perfect''
    --- notice the small gap in this case --- but it's pretty good,
    and simple/consistent; it's easy to predict what will happen.}
\end{frame}

%\begin{frame}{}
%  \begin{center}
%    \includegraphics[width=3in]{images/two.pdf}
%  \end{center}
%\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/linked.pdf}
  \end{center}
  \stext{Here's something else that the current version gets
    wrong---there's no way to refer to previously laid out diagrams,
    so making a diagram like this one is very tedious; there's no good
  way to do it.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/expressions.pdf}
  \end{center}
  \stext{In the new version, every diagram will have an implicit
    distinguished ``control point'' thought of as the origin of a
    local coordinate system.  Other points can be defined and named
    relative to the origin and each other, using a simple language for
    linear expressions.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/expressions-join.pdf}
  \end{center}
  \stext{We can compose two diagrams by identifying a point from each,
    with the identified point becoming the new origin.  All other
    diagram combinators can be implemented in terms of this operation.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/three.pdf}
  \end{center}
  \stext{A third limitation of the first version is that it only works
    for two-dimensional diagrams!}
\end{frame}

\begin{frame}{}
  \begin{center}
      \Huge $\R^2 \qquad \R^3 \qquad \R \to V \dots$
      \includegraphics[width=1in]{images/R2.pdf}
      \includegraphics[width=1.5in]{images/3D-axis.png}
      \includegraphics[width=1.5in]{images/clock.jpg}
  \end{center}
  \stext{The new version is polymorphic over the vector space used, so
    we can have 2D diagrams, 3D diagrams, animations\dots}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/four.pdf}
  \end{center}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/cairo.pdf}
  \end{center}
  \stext{Something else the original version got wrong was to require
    Cairo as a rendering backend.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/backends.pdf}
  \end{center}
  \stext{The new version will be more modular, allowing anyone to
    easily create a new rendering backend.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/help-wanted.jpg}
  \end{center}
  \stext{How you can help:}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/help-1.pdf}
  \end{center}
  \stext{Help work on the core library.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/help-2.pdf}
  \end{center}
  \stext{Help write a blessed standard library of convenient/common things
    implemented in terms of core primitives.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/help-3.pdf}
  \end{center}
  \stext{Write a backend.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/help-4.pdf}
  \end{center}
  \stext{Write documentation or a tutorial.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/help-5.pdf}
  \end{center}
  \stext{Make some examples for fun and to help drive development.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/help-6.pdf}
  \end{center}
  \stext{Write a higher-level extension library.}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=3in]{images/help-7.pdf}
  \end{center}
  \stext{Write an application for real-time visualization/editing, or
    a gitit plugin, or\dots}
\end{frame}

\begin{frame}{}
  \begin{center}
    \includegraphics[width=1.5in]{images/logo.png}
  \end{center}
  \stext{\dots design a better logo.}
\end{frame}

\end{document}

