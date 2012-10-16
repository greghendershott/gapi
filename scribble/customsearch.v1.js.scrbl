#lang scribble/manual
Hi hi hi
@(require planet/scribble (for-label racket))
@title{CustomSearch API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you search over a website or collection of websites
@hyperlink["https://developers.google.com/custom-search/v1/using_rest" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "customsearch.v1.js")]
@section{API Parameters}
The following optional keyword arguments may be passed to all functions for this web service:
@defproc[(_
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]

) jsexpr?]{
@margin-note{This is not actually a function. This is just using Scribble's defproc form to list the optional keyword arguments that may be passed to @italic{all} functions for this service.}
@racket[fields]: Selector specifying which fields to include in a partial response.

@racket[key]: API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.

@racket[alt]: Data format for the response.

@racket[oauth_token]: OAuth 2.0 token for the current user.

@racket[prettyPrint]: Returns response with indentations and line breaks.

@racket[quotaUser]: Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.

@racket[userIp]: IP address of the site where the request originates. Use this if you want to enforce per-user limits.


}
@section{Resources}

@subsection{cse}
@defproc[(search-cse-list
[#:q q string?]
[#:sort sort string? 'N/A]
[#:filter filter string? 'N/A]
[#:start start string? 'N/A]
[#:cr cr string? 'N/A]
[#:cref cref string? 'N/A]
[#:cx cx string? 'N/A]
[#:dateRestrict dateRestrict string? 'N/A]
[#:exactTerms exactTerms string? 'N/A]
[#:excludeTerms excludeTerms string? 'N/A]
[#:fileType fileType string? 'N/A]
[#:gl gl string? 'N/A]
[#:highRange highRange string? 'N/A]
[#:hl hl string? 'N/A]
[#:hq hq string? 'N/A]
[#:imgColorType imgColorType string? 'N/A]
[#:imgDominantColor imgDominantColor string? 'N/A]
[#:imgSize imgSize string? 'N/A]
[#:imgType imgType string? 'N/A]
[#:linkSite linkSite string? 'N/A]
[#:lowRange lowRange string? 'N/A]
[#:orTerms orTerms string? 'N/A]
[#:relatedSite relatedSite string? 'N/A]
[#:rights rights string? 'N/A]
[#:safe safe string? 'N/A]
[#:searchType searchType string? 'N/A]
[#:siteSearch siteSearch string? 'N/A]
[#:siteSearchFilter siteSearchFilter string? 'N/A]
[#:c2coff c2coff string? 'N/A]
[#:googlehost googlehost string? 'N/A]
[#:lr lr string? 'N/A]
[#:num num string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns metadata about the search performed, metadata about the custom search engine used for the search, and the search results.

@racket[q]: Query

@racket[sort]: The sort expression to apply to the results

@racket[filter]: Controls turning on or off the duplicate content filter.

@racket[start]: The index of the first result to return

@racket[cr]: Country restrict(s).

@racket[cref]: The URL of a linked custom search engine

@racket[cx]: The custom search engine ID to scope this search query

@racket[dateRestrict]: Specifies all search results are from a time period

@racket[exactTerms]: Identifies a phrase that all documents in the search results must contain

@racket[excludeTerms]: Identifies a word or phrase that should not appear in any documents in the search results

@racket[fileType]: Returns images of a specified type. Some of the allowed values are: bmp, gif, png, jpg, svg, pdf, ...

@racket[gl]: Geolocation of end user.

@racket[highRange]: Creates a range in form as_nlo value..as_nhi value and attempts to append it to query

@racket[hl]: Sets the user interface language.

@racket[hq]: Appends the extra query terms to the query.

@racket[imgColorType]: Returns black and white, grayscale, or color images: mono, gray, and color.

@racket[imgDominantColor]: Returns images of a specific dominant color: yellow, green, teal, blue, purple, pink, white, gray, black and brown.

@racket[imgSize]: Returns images of a specified size, where size can be one of: icon, small, medium, large, xlarge, xxlarge, and huge.

@racket[imgType]: Returns images of a type, which can be one of: clipart, face, lineart, news, and photo.

@racket[linkSite]: Specifies that all search results should contain a link to a particular URL

@racket[lowRange]: Creates a range in form as_nlo value..as_nhi value and attempts to append it to query

@racket[orTerms]: Provides additional search terms to check for in a document, where each document in the search results must contain at least one of the additional search terms

@racket[relatedSite]: Specifies that all search results should be pages that are related to the specified URL

@racket[rights]: Filters based on licensing. Supported values include: cc_publicdomain, cc_attribute, cc_sharealike, cc_noncommercial, cc_nonderived and combinations of these.

@racket[safe]: Search safety level

@racket[searchType]: Specifies the search type: image.

@racket[siteSearch]: Specifies all search results should be pages from a given site

@racket[siteSearchFilter]: Controls whether to include or exclude results from the site named in the as_sitesearch parameter

@racket[c2coff]: Turns off the translation between zh-CN and zh-TW.

@racket[googlehost]: The local Google domain to use to perform the search.

@racket[lr]: The language restriction for the search results

@racket[num]: Number of search results to return

}

