#include packamon.disclaimer

#include packamon.from

#include packamon.system-dependencies

#include packamon.r-repos

#include packamon.r-dependencies

#include packamon.local-r-dependencies
COPY glpgStyle_*.tar.gz /tmp/glpgStyle.tar.gz
RUN R -e "install.packages('/tmp/glpgStyle.tar.gz', repos = NULL, dependencies = FALSE)"
COPY glpgUtilityFct_*.tar.gz /tmp/glpgUtilityFct.tar.gz
RUN R -e "install.packages('/tmp/glpgUtilityFct.tar.gz', repos = NULL, dependencies = FALSE)"

#include packamon.runtime-settings
