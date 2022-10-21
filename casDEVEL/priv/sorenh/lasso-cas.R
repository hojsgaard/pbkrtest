install_github("r-cas/caracas")
library(caracas)

options("width"=200)
options(caracas.print.ascii = TRUE)
options(caracas.print.prettyascii = TRUE)


st <- tex(Si)

S <- matrix_sym_symmetric(3, "s")
st <- tex(S)

texdump(S)

texdump <- function(x, name="obj"){

    tex_name <- paste0("_dump_", name, ".tex", collapes="")    
    s1 <- c("\\documentclass{article}",
            "\\usepackage{amsmath}", 
            "\\begin{document}",
            "\\["
            )
    
    s2 <- c("\\]",
            "\\end{document}")
    
    s1 <- paste0(s1, "\n")
    s2 <- paste0(s2, "\n")

    st_all <- paste0(c(s1, st, s2), collapse=" ")

    cat(st_all, file=tex_name)
    if (require(tinytex))
        tinytex::pdflatex(tex_name)
    invisible()
}


texdump(S)









paste0(s1, collapse=" ")

tinytex::pdflatex("_dump.tex")






cat("

  \\left[
    \begin{matrix}
      s_{11} & s_{21} & s_{31}\\
      s_{21} & s_{22} & s_{32}\\
      s_{31} & s_{32} & s_{33}
    \\end{matrix}
  \\right]

\\]
\\end{document}",




## load_all("../caracas")

S <- matrix_sym_symmetric(3, "s")
S
W <- S
idx = 1:3

##' Regress 1 on 2,3
##' 

i   = 1
ni  = idx[-i]

beta <- inv(S[ni, ni]) %*% S[i, ni]  |> simplify()

W[i, ni] <- beta
W[ni, i] <- beta
W

##' Regress 2 on 1,3
i   = 2
ni  = idx[-i]

beta <- inv(S[ni, ni]) %*% S[i, ni]  |> simplify()

W[i, ni] <- beta
W[ni, i] <- beta

W

## Regress 3 on 1,2

i   = 3
ni  = idx[-i]

beta <- inv(S[ni, ni]) %*% S[i, ni]  |> simplify()

W[i, ni] <- beta
W[ni, i] <- beta

W

Si <- inv(S) |> simplify() 
Si
