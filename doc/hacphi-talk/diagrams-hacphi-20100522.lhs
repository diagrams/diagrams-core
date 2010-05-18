%% -*- LaTeX -*-
\documentclass[xcolor=svgnames]{beamer}

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

\title{Declarative, embedded drawing}
\date{Hac $\varphi$ \\ May 22, 2010}
\author{Brent Yorgey \\ University of Pennsylvania}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

 \begin{frame}{}
   \titlepage
 \end{frame}



\end{document}

