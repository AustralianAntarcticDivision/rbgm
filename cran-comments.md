## Test environments
* local Windows, R 3.3.1
* local debian 16.04, R 3.3.1
* win-builder (devel)

## R CMD check results

> * checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Michael D. Sumner <mdsumner@gmail.com>'
> 
> New submission
> 
> Possibly mis-spelled words in DESCRIPTION:
>   BGM (3:40, 13:71, 15:38)
>   biogeochemical (14:38)
> 
> Found the following (possibly) invalid URLs:
>   URL: http://atlantis.cmar.csiro.au/
>     From: DESCRIPTION
>           inst/doc/BGM_Spatial.html
>     Status: 403
>     Message: Forbidden
>   URL: http://cran.r-project.org/web/packages/rbgm/index.html
>     From: README.md
>     Status: 404
>     Message: Not Found
>     CRAN URL not in canonical form
>   URL: https://cran.r-project.org/package=rbgm
>     From: README.md
>     Status: 404
>     Message: Not Found
>   The canonical URL of the CRAN page for a package is 
>   https://cran.r-project.org/package=pkgname
> 

* This is a new release.
* The spellings are intended. 
* The URLS do exist - or will once this is on CRAN.
* The non-canonical form of the CRAN URL in README.md is as per devtools template. 

Thanks very much!

Cheers, Mike. 

