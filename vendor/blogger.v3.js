{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "blogger:v3",
 "name": "blogger",
 "version": "v3",
 "revision": "20120508",
 "title": "Blogger API",
 "description": "API for access to the data within Blogger.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/blogger-16.png",
  "x32": "http://www.google.com/images/icons/product/blogger-32.png"
 },
 "documentationLink": "https://developers.google.com/blogger/docs/3.0/getting_started",
 "labels": [
  "limited_availability"
 ],
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/blogger/v3/",
 "basePath": "/blogger/v3/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "blogger/v3/",
 "batchPath": "batch",
 "parameters": {
  "alt": {
   "type": "string",
   "description": "Data format for the response.",
   "default": "json",
   "enum": [
    "json"
   ],
   "enumDescriptions": [
    "Responses with Content-Type of application/json"
   ],
   "location": "query"
  },
  "fields": {
   "type": "string",
   "description": "Selector specifying which fields to include in a partial response.",
   "location": "query"
  },
  "key": {
   "type": "string",
   "description": "API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.",
   "location": "query"
  },
  "oauth_token": {
   "type": "string",
   "description": "OAuth 2.0 token for the current user.",
   "location": "query"
  },
  "prettyPrint": {
   "type": "boolean",
   "description": "Returns response with indentations and line breaks.",
   "default": "true",
   "location": "query"
  },
  "quotaUser": {
   "type": "string",
   "description": "Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.",
   "location": "query"
  },
  "userIp": {
   "type": "string",
   "description": "IP address of the site where the request originates. Use this if you want to enforce per-user limits.",
   "location": "query"
  }
 },
 "auth": {
  "oauth2": {
   "scopes": {
    "https://www.googleapis.com/auth/blogger": {
     "description": "Manage your Blogger account"
    },
    "https://www.googleapis.com/auth/blogger.readonly": {
     "description": "View your Blogger account"
    }
   }
  }
 },
 "schemas": {
  "Blog": {
   "id": "Blog",
   "type": "object",
   "properties": {
    "customMetaData": {
     "type": "string",
     "description": "The JSON custom meta-data for the Blog"
    },
    "description": {
     "type": "string",
     "description": "The description of this blog. This is displayed underneath the title."
    },
    "id": {
     "type": "string",
     "description": "The identifier for this resource.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entry. Always blogger#blog",
     "default": "blogger#blog"
    },
    "locale": {
     "type": "object",
     "description": "The locale this Blog is set to.",
     "properties": {
      "country": {
       "type": "string",
       "description": "The country this blog's locale is set to."
      },
      "language": {
       "type": "string",
       "description": "The language this blog is authored in."
      },
      "variant": {
       "type": "string",
       "description": "The language variant this blog is authored in."
      }
     }
    },
    "name": {
     "type": "string",
     "description": "The name of this blog. This is displayed as the title."
    },
    "pages": {
     "type": "object",
     "description": "The container of pages in this blog.",
     "properties": {
      "selfLink": {
       "type": "string",
       "description": "The URL of the container for pages in this blog."
      },
      "totalItems": {
       "type": "integer",
       "description": "The count of pages in this blog.",
       "format": "int32"
      }
     }
    },
    "posts": {
     "type": "object",
     "description": "The container of posts in this blog.",
     "properties": {
      "items": {
       "type": "array",
       "description": "The List of Posts for this Blog.",
       "items": {
        "$ref": "Post"
       }
      },
      "selfLink": {
       "type": "string",
       "description": "The URL of the container for posts in this blog."
      },
      "totalItems": {
       "type": "integer",
       "description": "The count of posts in this blog.",
       "format": "int32"
      }
     }
    },
    "published": {
     "type": "string",
     "description": "RFC 3339 date-time when this blog was published.",
     "format": "date-time"
    },
    "selfLink": {
     "type": "string",
     "description": "The API REST URL to fetch this resource from."
    },
    "updated": {
     "type": "string",
     "description": "RFC 3339 date-time when this blog was last updated.",
     "format": "date-time"
    },
    "url": {
     "type": "string",
     "description": "The URL where this blog is published."
    }
   }
  },
  "BlogList": {
   "id": "BlogList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The list of Blogs this user has Authorship or Admin rights over.",
     "items": {
      "$ref": "Blog"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entity. Always blogger#blogList",
     "default": "blogger#blogList"
    }
   }
  },
  "Comment": {
   "id": "Comment",
   "type": "object",
   "properties": {
    "author": {
     "type": "object",
     "description": "The author of this Comment.",
     "properties": {
      "displayName": {
       "type": "string",
       "description": "The display name."
      },
      "id": {
       "type": "string",
       "description": "The identifier of the Comment creator."
      },
      "image": {
       "type": "object",
       "description": "The comment creator's avatar.",
       "properties": {
        "url": {
         "type": "string",
         "description": "The comment creator's avatar URL."
        }
       }
      },
      "url": {
       "type": "string",
       "description": "The URL of the Comment creator's Profile page."
      }
     }
    },
    "blog": {
     "type": "object",
     "description": "Data about the blog containing this comment.",
     "properties": {
      "id": {
       "type": "string",
       "description": "The identifier of the blog containing this comment.",
       "format": "int64"
      }
     }
    },
    "content": {
     "type": "string",
     "description": "The actual content of the comment. May include HTML markup."
    },
    "id": {
     "type": "string",
     "description": "The identifier for this resource.",
     "format": "int64"
    },
    "inReplyTo": {
     "type": "object",
     "description": "Data about the comment this is in reply to.",
     "properties": {
      "id": {
       "type": "string",
       "description": "The identified of the parent of this comment.",
       "format": "int64"
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entry. Always blogger#comment",
     "default": "blogger#comment"
    },
    "post": {
     "type": "object",
     "description": "Data about the post containing this comment.",
     "properties": {
      "id": {
       "type": "string",
       "description": "The identifier of the post containing this comment.",
       "format": "int64"
      }
     }
    },
    "published": {
     "type": "string",
     "description": "RFC 3339 date-time when this comment was published.",
     "format": "date-time"
    },
    "selfLink": {
     "type": "string",
     "description": "The API REST URL to fetch this resource from."
    },
    "updated": {
     "type": "string",
     "description": "RFC 3339 date-time when this comment was last updated.",
     "format": "date-time"
    }
   }
  },
  "CommentList": {
   "id": "CommentList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The List of Comments for a Post.",
     "items": {
      "$ref": "Comment"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entry. Always blogger#commentList",
     "default": "blogger#commentList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Pagination token to fetch the next page, if one exists."
    },
    "prevPageToken": {
     "type": "string",
     "description": "Pagination token to fetch the previous page, if one exists."
    }
   }
  },
  "Page": {
   "id": "Page",
   "type": "object",
   "properties": {
    "author": {
     "type": "object",
     "description": "The author of this Page.",
     "properties": {
      "displayName": {
       "type": "string",
       "description": "The display name."
      },
      "id": {
       "type": "string",
       "description": "The identifier of the Page creator."
      },
      "image": {
       "type": "object",
       "description": "The page author's avatar.",
       "properties": {
        "url": {
         "type": "string",
         "description": "The page author's avatar URL."
        }
       }
      },
      "url": {
       "type": "string",
       "description": "The URL of the Page creator's Profile page."
      }
     }
    },
    "blog": {
     "type": "object",
     "description": "Data about the blog containing this Page.",
     "properties": {
      "id": {
       "type": "string",
       "description": "The identifier of the blog containing this page.",
       "format": "int64"
      }
     }
    },
    "content": {
     "type": "string",
     "description": "The body content of this Page, in HTML."
    },
    "id": {
     "type": "string",
     "description": "The identifier for this resource.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entity. Always blogger#page",
     "default": "blogger#page"
    },
    "published": {
     "type": "string",
     "description": "RFC 3339 date-time when this Page was published.",
     "format": "date-time"
    },
    "selfLink": {
     "type": "string",
     "description": "The API REST URL to fetch this resource from."
    },
    "title": {
     "type": "string",
     "description": "The title of this entity. This is the name displayed in the Admin user interface."
    },
    "updated": {
     "type": "string",
     "description": "RFC 3339 date-time when this Page was last updated.",
     "format": "date-time"
    },
    "url": {
     "type": "string",
     "description": "The URL that this Page is displayed at."
    }
   }
  },
  "PageList": {
   "id": "PageList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The list of Pages for a Blog.",
     "items": {
      "$ref": "Page"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entity. Always blogger#pageList",
     "default": "blogger#pageList"
    }
   }
  },
  "Post": {
   "id": "Post",
   "type": "object",
   "properties": {
    "author": {
     "type": "object",
     "description": "The author of this Post.",
     "properties": {
      "displayName": {
       "type": "string",
       "description": "The display name."
      },
      "id": {
       "type": "string",
       "description": "The identifier of the Post creator."
      },
      "image": {
       "type": "object",
       "description": "The Post author's avatar.",
       "properties": {
        "url": {
         "type": "string",
         "description": "The Post author's avatar URL."
        }
       }
      },
      "url": {
       "type": "string",
       "description": "The URL of the Post creator's Profile page."
      }
     }
    },
    "blog": {
     "type": "object",
     "description": "Data about the blog containing this Post.",
     "properties": {
      "id": {
       "type": "string",
       "description": "The identifier of the Blog that contains this Post.",
       "format": "int64"
      }
     }
    },
    "content": {
     "type": "string",
     "description": "The content of the Post. May contain HTML markup."
    },
    "customMetaData": {
     "type": "string",
     "description": "The JSON meta-data for the Post."
    },
    "id": {
     "type": "string",
     "description": "The identifier of this Post.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entity. Always blogger#post",
     "default": "blogger#post"
    },
    "labels": {
     "type": "array",
     "description": "The list of labels this Post was tagged with.",
     "items": {
      "type": "string"
     }
    },
    "location": {
     "type": "object",
     "description": "The location for geotagged posts.",
     "properties": {
      "lat": {
       "type": "number",
       "description": "Location's latitude.",
       "format": "double"
      },
      "lng": {
       "type": "number",
       "description": "Location's longitude.",
       "format": "double"
      },
      "name": {
       "type": "string",
       "description": "Location name."
      },
      "span": {
       "type": "string",
       "description": "Location's viewport span. Can be used when rendering a map preview."
      }
     }
    },
    "published": {
     "type": "string",
     "description": "RFC 3339 date-time when this Post was published.",
     "format": "date-time"
    },
    "replies": {
     "type": "object",
     "description": "The container of comments on this Post.",
     "properties": {
      "items": {
       "type": "array",
       "description": "The List of Comments for this Post.",
       "items": {
        "$ref": "Comment"
       }
      },
      "selfLink": {
       "type": "string",
       "description": "The URL of the comments on this post."
      },
      "totalItems": {
       "type": "string",
       "description": "The count of comments on this post.",
       "format": "int64"
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "The API REST URL to fetch this resource from."
    },
    "title": {
     "type": "string",
     "description": "The title of the Post."
    },
    "updated": {
     "type": "string",
     "description": "RFC 3339 date-time when this Post was last updated.",
     "format": "date-time"
    },
    "url": {
     "type": "string",
     "description": "The URL where this Post is displayed."
    }
   }
  },
  "PostList": {
   "id": "PostList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "The list of Posts for this Blog.",
     "items": {
      "$ref": "Post"
     }
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entity. Always blogger#postList",
     "default": "blogger#postList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Pagination token to fetch the next page, if one exists."
    },
    "prevPageToken": {
     "type": "string",
     "description": "Pagination token to fetch the previous page, if one exists."
    }
   }
  },
  "User": {
   "id": "User",
   "type": "object",
   "properties": {
    "about": {
     "type": "string",
     "description": "Profile summary information."
    },
    "blogs": {
     "type": "object",
     "description": "The container of blogs for this user.",
     "properties": {
      "selfLink": {
       "type": "string",
       "description": "The URL of the Blogs for this user."
      }
     }
    },
    "created": {
     "type": "string",
     "description": "The timestamp of when this profile was created, in seconds since epoch.",
     "format": "date-time"
    },
    "displayName": {
     "type": "string",
     "description": "The display name."
    },
    "id": {
     "type": "string",
     "description": "The identifier for this User."
    },
    "kind": {
     "type": "string",
     "description": "The kind of this entity. Always blogger#user",
     "default": "blogger#user"
    },
    "locale": {
     "type": "object",
     "description": "This user's locale",
     "properties": {
      "country": {
       "type": "string",
       "description": "The user's country setting."
      },
      "language": {
       "type": "string",
       "description": "The user's language setting."
      },
      "variant": {
       "type": "string",
       "description": "The user's language variant setting."
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "The API REST URL to fetch this resource from."
    },
    "url": {
     "type": "string",
     "description": "The user's profile page."
    }
   }
  }
 },
 "resources": {
  "blogs": {
   "methods": {
    "get": {
     "id": "blogger.blogs.get",
     "path": "blogs/{blogId}",
     "httpMethod": "GET",
     "description": "Gets one blog by id.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "The ID of the blog to get.",
       "required": true,
       "location": "path"
      },
      "maxPosts": {
       "type": "integer",
       "description": "Maximum number of posts to pull back with the blog.",
       "format": "uint32",
       "location": "query"
      }
     },
     "parameterOrder": [
      "blogId"
     ],
     "response": {
      "$ref": "Blog"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    },
    "getByUrl": {
     "id": "blogger.blogs.getByUrl",
     "path": "blogs/byurl",
     "httpMethod": "GET",
     "description": "Retrieve a Blog by URL.",
     "parameters": {
      "url": {
       "type": "string",
       "description": "The URL of the blog to retrieve.",
       "location": "query"
      }
     },
     "response": {
      "$ref": "Blog"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    },
    "listByUser": {
     "id": "blogger.blogs.listByUser",
     "path": "users/{userId}/blogs",
     "httpMethod": "GET",
     "description": "Retrieves a list of blogs, possibly filtered.",
     "parameters": {
      "userId": {
       "type": "string",
       "description": "ID of the user whose blogs are to be fetched. Either the word 'self' (sans quote marks) or the user's profile identifier.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId"
     ],
     "response": {
      "$ref": "BlogList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    }
   }
  },
  "comments": {
   "methods": {
    "get": {
     "id": "blogger.comments.get",
     "path": "blogs/{blogId}/posts/{postId}/comments/{commentId}",
     "httpMethod": "GET",
     "description": "Gets one comment by id.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog to containing the comment.",
       "required": true,
       "location": "path"
      },
      "commentId": {
       "type": "string",
       "description": "The ID of the comment to get.",
       "required": true,
       "location": "path"
      },
      "postId": {
       "type": "string",
       "description": "ID of the post to fetch posts from.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "blogId",
      "postId",
      "commentId"
     ],
     "response": {
      "$ref": "Comment"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    },
    "list": {
     "id": "blogger.comments.list",
     "path": "blogs/{blogId}/posts/{postId}/comments",
     "httpMethod": "GET",
     "description": "Retrieves the comments for a blog, possibly filtered.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog to fetch comments from.",
       "required": true,
       "location": "path"
      },
      "endDate": {
       "type": "string",
       "description": "Latest date of comment to fetch, a date-time with RFC 3339 formatting.",
       "format": "date-time",
       "location": "query"
      },
      "fetchBodies": {
       "type": "boolean",
       "description": "Whether the body content of the comments is included.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of comments to include in the result.",
       "format": "uint32",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Continuation token if request is paged.",
       "location": "query"
      },
      "postId": {
       "type": "string",
       "description": "ID of the post to fetch posts from.",
       "required": true,
       "location": "path"
      },
      "startDate": {
       "type": "string",
       "description": "Earliest date of comment to fetch, a date-time with RFC 3339 formatting.",
       "format": "date-time",
       "location": "query"
      }
     },
     "parameterOrder": [
      "blogId",
      "postId"
     ],
     "response": {
      "$ref": "CommentList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    }
   }
  },
  "pages": {
   "methods": {
    "get": {
     "id": "blogger.pages.get",
     "path": "blogs/{blogId}/pages/{pageId}",
     "httpMethod": "GET",
     "description": "Gets one blog page by id.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog containing the page.",
       "required": true,
       "location": "path"
      },
      "pageId": {
       "type": "string",
       "description": "The ID of the page to get.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "blogId",
      "pageId"
     ],
     "response": {
      "$ref": "Page"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    },
    "list": {
     "id": "blogger.pages.list",
     "path": "blogs/{blogId}/pages",
     "httpMethod": "GET",
     "description": "Retrieves pages for a blog, possibly filtered.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog to fetch pages from.",
       "required": true,
       "location": "path"
      },
      "fetchBodies": {
       "type": "boolean",
       "description": "Whether to retrieve the Page bodies.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "blogId"
     ],
     "response": {
      "$ref": "PageList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    }
   }
  },
  "posts": {
   "methods": {
    "delete": {
     "id": "blogger.posts.delete",
     "path": "blogs/{blogId}/posts/{postId}",
     "httpMethod": "DELETE",
     "description": "Delete a post by id.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "The Id of the Blog.",
       "required": true,
       "location": "path"
      },
      "postId": {
       "type": "string",
       "description": "The ID of the Post.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "blogId",
      "postId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/blogger"
     ]
    },
    "get": {
     "id": "blogger.posts.get",
     "path": "blogs/{blogId}/posts/{postId}",
     "httpMethod": "GET",
     "description": "Get a post by id.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog to fetch the post from.",
       "required": true,
       "location": "path"
      },
      "maxComments": {
       "type": "integer",
       "description": "Maximum number of comments to pull back on a post.",
       "format": "uint32",
       "location": "query"
      },
      "postId": {
       "type": "string",
       "description": "The ID of the post",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "blogId",
      "postId"
     ],
     "response": {
      "$ref": "Post"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    },
    "getByPath": {
     "id": "blogger.posts.getByPath",
     "path": "blogs/{blogId}/posts/bypath",
     "httpMethod": "GET",
     "description": "Retrieve a Post by Path.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog to fetch the post from.",
       "required": true,
       "location": "path"
      },
      "maxComments": {
       "type": "integer",
       "description": "Maximum number of comments to pull back on a post.",
       "format": "uint32",
       "location": "query"
      },
      "path": {
       "type": "string",
       "description": "Path of the Post to retrieve.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "blogId"
     ],
     "response": {
      "$ref": "Post"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    },
    "insert": {
     "id": "blogger.posts.insert",
     "path": "blogs/{blogId}/posts",
     "httpMethod": "POST",
     "description": "Add a post.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog to fetch the post from.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "blogId"
     ],
     "request": {
      "$ref": "Post"
     },
     "response": {
      "$ref": "Post"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger"
     ]
    },
    "list": {
     "id": "blogger.posts.list",
     "path": "blogs/{blogId}/posts",
     "httpMethod": "GET",
     "description": "Retrieves a list of posts, possibly filtered.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog to fetch posts from.",
       "required": true,
       "location": "path"
      },
      "endDate": {
       "type": "string",
       "description": "Latest post date to fetch, a date-time with RFC 3339 formatting.",
       "format": "date-time",
       "location": "query"
      },
      "fetchBodies": {
       "type": "boolean",
       "description": "Whether the body content of posts is included.",
       "location": "query"
      },
      "labels": {
       "type": "string",
       "description": "Comma-separated list of labels to search for.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of posts to fetch.",
       "format": "uint32",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Continuation token if the request is paged.",
       "location": "query"
      },
      "startDate": {
       "type": "string",
       "description": "Earliest post date to fetch, a date-time with RFC 3339 formatting.",
       "format": "date-time",
       "location": "query"
      }
     },
     "parameterOrder": [
      "blogId"
     ],
     "response": {
      "$ref": "PostList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    },
    "patch": {
     "id": "blogger.posts.patch",
     "path": "blogs/{blogId}/posts/{postId}",
     "httpMethod": "PATCH",
     "description": "Update a post. This method supports patch semantics.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "The ID of the Blog.",
       "required": true,
       "location": "path"
      },
      "postId": {
       "type": "string",
       "description": "The ID of the Post.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "blogId",
      "postId"
     ],
     "request": {
      "$ref": "Post"
     },
     "response": {
      "$ref": "Post"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger"
     ]
    },
    "search": {
     "id": "blogger.posts.search",
     "path": "blogs/{blogId}/posts/search",
     "httpMethod": "GET",
     "description": "Search for a post.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "ID of the blog to fetch the post from.",
       "required": true,
       "location": "path"
      },
      "q": {
       "type": "string",
       "description": "Query terms to search this blog for matching posts.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "blogId"
     ],
     "response": {
      "$ref": "PostList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    },
    "update": {
     "id": "blogger.posts.update",
     "path": "blogs/{blogId}/posts/{postId}",
     "httpMethod": "PUT",
     "description": "Update a post.",
     "parameters": {
      "blogId": {
       "type": "string",
       "description": "The ID of the Blog.",
       "required": true,
       "location": "path"
      },
      "postId": {
       "type": "string",
       "description": "The ID of the Post.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "blogId",
      "postId"
     ],
     "request": {
      "$ref": "Post"
     },
     "response": {
      "$ref": "Post"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger"
     ]
    }
   }
  },
  "users": {
   "methods": {
    "get": {
     "id": "blogger.users.get",
     "path": "users/{userId}",
     "httpMethod": "GET",
     "description": "Gets one user by id.",
     "parameters": {
      "userId": {
       "type": "string",
       "description": "The ID of the user to get.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId"
     ],
     "response": {
      "$ref": "User"
     },
     "scopes": [
      "https://www.googleapis.com/auth/blogger",
      "https://www.googleapis.com/auth/blogger.readonly"
     ]
    }
   }
  }
 }
}
