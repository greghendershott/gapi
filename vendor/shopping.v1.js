{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "shopping:v1",
 "name": "shopping",
 "version": "v1",
 "revision": "20120904",
 "title": "Search API For Shopping",
 "description": "Lets you search over product data.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/search-16.gif",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "https://developers.google.com/shopping-search/v1/getting_started",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/shopping/search/v1/",
 "basePath": "/shopping/search/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "shopping/search/v1/",
 "batchPath": "batch",
 "parameters": {
  "alt": {
   "type": "string",
   "description": "Data format for the response.",
   "default": "json",
   "enum": [
    "atom",
    "json"
   ],
   "enumDescriptions": [
    "Responses with Content-Type of application/atom+xml",
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
    "https://www.googleapis.com/auth/shoppingapi": {
     "description": "View your product data"
    }
   }
  }
 },
 "schemas": {
  "Product": {
   "id": "Product",
   "type": "object",
   "properties": {
    "categories": {
     "type": "array",
     "description": "List of categories for product.",
     "items": {
      "$ref": "ShoppingModelCategoryJsonV1"
     }
    },
    "debug": {
     "$ref": "ShoppingModelDebugJsonV1",
     "description": "Google internal."
    },
    "id": {
     "type": "string",
     "description": "Id of product."
    },
    "kind": {
     "type": "string",
     "description": "The kind of item, always shopping#product.",
     "default": "shopping#product"
    },
    "product": {
     "$ref": "ShoppingModelProductJsonV1",
     "description": "Product."
    },
    "recommendations": {
     "type": "array",
     "description": "Recommendations for product.",
     "items": {
      "$ref": "ShoppingModelRecommendationsJsonV1"
     }
    },
    "requestId": {
     "type": "string",
     "description": "Unique identifier for this request."
    },
    "selfLink": {
     "type": "string",
     "description": "Self link of product when generated for a lookup request. Self link of product when generated for a search request."
    }
   }
  },
  "Products": {
   "id": "Products",
   "type": "object",
   "properties": {
    "categories": {
     "type": "array",
     "description": "List of categories.",
     "items": {
      "$ref": "ShoppingModelCategoryJsonV1"
     }
    },
    "categoryRecommendations": {
     "type": "array",
     "description": "Recommendations for category.",
     "items": {
      "$ref": "ShoppingModelRecommendationsJsonV1"
     }
    },
    "currentItemCount": {
     "type": "integer",
     "description": "Current item count.",
     "format": "int32"
    },
    "debug": {
     "$ref": "ShoppingModelDebugJsonV1",
     "description": "Google internal."
    },
    "etag": {
     "type": "string",
     "description": "Etag of feed."
    },
    "facets": {
     "type": "array",
     "description": "List of facets.",
     "items": {
      "type": "object",
      "properties": {
       "buckets": {
        "type": "array",
        "description": "List of Buckets within facet.",
        "items": {
         "type": "object",
         "properties": {
          "count": {
           "type": "integer",
           "description": "Number of products matching the query that have a value for the facet's property or attribute that matches the bucket.",
           "format": "int32"
          },
          "max": {
           "type": "any",
           "description": "Upper bound of the bucket (omitted for value buckets or if the range has no upper bound)."
          },
          "maxExclusive": {
           "type": "boolean",
           "description": "Whether the upper bound of the bucket is exclusive (omitted for value buckets or if the range has no upper bound)."
          },
          "min": {
           "type": "any",
           "description": "Lower bound of the bucket (omitted for value buckets or if the range has no lower bound)."
          },
          "minExclusive": {
           "type": "boolean",
           "description": "Whether the lower bound of the bucket is exclusive (omitted for value buckets or if the range has no lower bound)."
          },
          "value": {
           "type": "any",
           "description": "Value of the bucket (omitted for range buckets)."
          }
         }
        }
       },
       "count": {
        "type": "integer",
        "description": "Number of products matching the query that have a value for the facet's property or attribute.",
        "format": "int32"
       },
       "displayName": {
        "type": "string",
        "description": "Display name of facet."
       },
       "name": {
        "type": "string",
        "description": "Name of the facet's attribute (omitted for property facets)."
       },
       "property": {
        "type": "string",
        "description": "Property of facet (omitted for attribute facets)."
       },
       "type": {
        "type": "string",
        "description": "Type of facet's attribute (omitted for property facets, one of: text, bool, int, float)."
       },
       "unit": {
        "type": "string",
        "description": "Unit of the facet's property or attribute (omitted if the facet's property or attribute has no unit)."
       }
      }
     }
    },
    "id": {
     "type": "string",
     "description": "Id of feed.",
     "default": "tag:google.com,2010:shopping/products"
    },
    "items": {
     "type": "array",
     "description": "List of returned products.",
     "items": {
      "$ref": "Product"
     }
    },
    "itemsPerPage": {
     "type": "integer",
     "description": "Number of items per page of results.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "The fixed string \"shopping#products\". The kind of feed returned.",
     "default": "shopping#products"
    },
    "nextLink": {
     "type": "string",
     "description": "Next link of feed."
    },
    "previousLink": {
     "type": "string",
     "description": "Previous link of feed."
    },
    "promotions": {
     "type": "array",
     "description": "List of promotions.",
     "items": {
      "type": "object",
      "properties": {
       "customFields": {
        "type": "array",
        "description": "List of custom fields of promotion.",
        "items": {
         "type": "object",
         "properties": {
          "name": {
           "type": "string",
           "description": "Name of field."
          },
          "value": {
           "type": "string",
           "description": "Value of field."
          }
         }
        }
       },
       "customHtml": {
        "type": "string",
        "description": "Custom HTML of promotion (omitted if type is not custom)."
       },
       "description": {
        "type": "string",
        "description": "Description of promotion (omitted if type is not standard)."
       },
       "destLink": {
        "type": "string",
        "description": "Link to promotion (omitted if type is not standard)."
       },
       "imageLink": {
        "type": "string",
        "description": "Link to promotion image (omitted if type is not standard)."
       },
       "name": {
        "type": "string",
        "description": "Name of promotion (omitted if type is not standard)."
       },
       "product": {
        "$ref": "ShoppingModelProductJsonV1",
        "description": "Product of promotion (omitted if type is not product)."
       },
       "type": {
        "type": "string",
        "description": "Type of promotion (one of: standard, product, custom)."
       }
      }
     }
    },
    "redirects": {
     "type": "array",
     "description": "Redirects.",
     "items": {
      "type": "string"
     }
    },
    "relatedQueries": {
     "type": "array",
     "description": "Related queries.",
     "items": {
      "type": "string"
     }
    },
    "requestId": {
     "type": "string",
     "description": "Unique identifier for this request."
    },
    "selfLink": {
     "type": "string",
     "description": "Self link of feed."
    },
    "spelling": {
     "type": "object",
     "description": "Spelling.",
     "properties": {
      "suggestion": {
       "type": "string",
       "description": "Suggestion for spelling."
      }
     }
    },
    "startIndex": {
     "type": "integer",
     "description": "1-based index of the first item in the search results.",
     "format": "int32"
    },
    "stores": {
     "type": "array",
     "description": "List of returned stores.",
     "items": {
      "type": "object",
      "properties": {
       "address": {
        "type": "string",
        "description": "Address of store."
       },
       "location": {
        "type": "string",
        "description": "Location of store."
       },
       "name": {
        "type": "string",
        "description": "Name of merchant."
       },
       "storeCode": {
        "type": "string",
        "description": "Merchant-supplied store code."
       },
       "storeId": {
        "type": "string",
        "description": "Id of store."
       },
       "storeName": {
        "type": "string",
        "description": "Name of store."
       },
       "telephone": {
        "type": "string",
        "description": "Telephone number of store."
       }
      }
     }
    },
    "totalItems": {
     "type": "integer",
     "description": "Total number of search results.",
     "format": "int32"
    }
   }
  },
  "ShoppingModelCategoryJsonV1": {
   "id": "ShoppingModelCategoryJsonV1",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Id of category."
    },
    "parents": {
     "type": "array",
     "description": "Ids of the parents of the category.",
     "items": {
      "type": "string"
     }
    },
    "shortName": {
     "type": "string",
     "description": "Short name of category."
    },
    "url": {
     "type": "string",
     "description": "URL of category."
    }
   }
  },
  "ShoppingModelDebugJsonV1": {
   "id": "ShoppingModelDebugJsonV1",
   "type": "object",
   "properties": {
    "backendTimes": {
     "type": "array",
     "description": "Google internal",
     "items": {
      "type": "object",
      "properties": {
       "elapsedMillis": {
        "type": "string",
        "description": "Google internal",
        "format": "int64"
       },
       "hostName": {
        "type": "string",
        "description": "Google internal"
       },
       "name": {
        "type": "string",
        "description": "Google internal"
       },
       "serverMillis": {
        "type": "string",
        "description": "Google internal",
        "format": "int64"
       }
      }
     }
    },
    "elapsedMillis": {
     "type": "string",
     "description": "Google internal.",
     "format": "int64"
    },
    "facetsRequest": {
     "type": "string",
     "description": "Google internal."
    },
    "facetsResponse": {
     "type": "string",
     "description": "Google internal."
    },
    "rdcResponse": {
     "type": "string",
     "description": "Google internal."
    },
    "recommendedItemsRequest": {
     "type": "string",
     "description": "Google internal."
    },
    "recommendedItemsResponse": {
     "type": "string",
     "description": "Google internal."
    },
    "searchRequest": {
     "type": "string",
     "description": "Google internal."
    },
    "searchResponse": {
     "type": "string",
     "description": "Google internal."
    }
   }
  },
  "ShoppingModelProductJsonV1": {
   "id": "ShoppingModelProductJsonV1",
   "type": "object",
   "properties": {
    "attributes": {
     "type": "array",
     "description": "Attributes of product (available only with a cx source).",
     "items": {
      "type": "object",
      "properties": {
       "displayName": {
        "type": "string",
        "description": "Display Name of prodct attribute."
       },
       "name": {
        "type": "string",
        "description": "Name of product attribute."
       },
       "type": {
        "type": "string",
        "description": "Type of product attribute (one of: text, bool, int, float, dateRange, url)."
       },
       "unit": {
        "type": "string",
        "description": "Unit of product attribute."
       },
       "value": {
        "type": "any",
        "description": "Value of product attribute."
       }
      }
     }
    },
    "author": {
     "type": "object",
     "description": "Author of product.",
     "properties": {
      "accountId": {
       "type": "string",
       "description": "Account id of product author.",
       "format": "uint64"
      },
      "name": {
       "type": "string",
       "description": "Name of product author."
      }
     }
    },
    "brand": {
     "type": "string",
     "description": "Brand of product."
    },
    "categories": {
     "type": "array",
     "description": "Categories of product according to the selected taxonomy, omitted if no taxonomy is selected.",
     "items": {
      "type": "string"
     }
    },
    "condition": {
     "type": "string",
     "description": "Condition of product (one of: new, refurbished, used)."
    },
    "country": {
     "type": "string",
     "description": "ISO 3166 code of target country of product."
    },
    "creationTime": {
     "type": "string",
     "description": "RFC 3339 formatted creation time and date of product.",
     "format": "date-time"
    },
    "description": {
     "type": "string",
     "description": "Description of product."
    },
    "googleId": {
     "type": "string",
     "description": "Google id of product.",
     "format": "uint64"
    },
    "gtin": {
     "type": "string",
     "description": "The first GTIN of the product. Deprecated in favor of \"gtins\"."
    },
    "gtins": {
     "type": "array",
     "description": "List of all the product's GTINs (in GTIN-14 format).",
     "items": {
      "type": "string"
     }
    },
    "images": {
     "type": "array",
     "description": "Images of product.",
     "items": {
      "type": "object",
      "properties": {
       "link": {
        "type": "string",
        "description": "Link to product image."
       },
       "status": {
        "type": "string"
       },
       "thumbnails": {
        "type": "array",
        "description": "Thumbnails of product image.",
        "items": {
         "type": "object",
         "properties": {
          "content": {
           "type": "string",
           "description": "Content of thumbnail (only available for the first thumbnail of the top results if SAYT is enabled)."
          },
          "height": {
           "type": "integer",
           "description": "Height of thumbnail (omitted if not specified in the request).",
           "format": "int32"
          },
          "link": {
           "type": "string",
           "description": "Link to thumbnail."
          },
          "width": {
           "type": "integer",
           "description": "Width of thumbnail (omitted if not specified in the request).",
           "format": "int32"
          }
         }
        }
       }
      }
     }
    },
    "internal1": {
     "type": "array",
     "description": "Google Internal.",
     "items": {
      "type": "string"
     }
    },
    "internal10": {
     "type": "array",
     "description": "Google Internal.",
     "items": {
      "type": "string"
     }
    },
    "internal12": {
     "type": "string",
     "description": "Google Internal."
    },
    "internal13": {
     "type": "number",
     "description": "Google Internal.",
     "format": "double"
    },
    "internal14": {
     "type": "number",
     "description": "Google Internal.",
     "format": "double"
    },
    "internal15": {
     "type": "number",
     "description": "Google Internal.",
     "format": "double"
    },
    "internal3": {
     "type": "string",
     "description": "Google Internal."
    },
    "internal4": {
     "type": "array",
     "description": "Google Internal.",
     "items": {
      "type": "object",
      "properties": {
       "confidence": {
        "type": "number",
        "description": "Google Internal.",
        "format": "double"
       },
       "node": {
        "type": "integer",
        "description": "Google Internal.",
        "format": "int32"
       }
      }
     }
    },
    "internal6": {
     "type": "string",
     "description": "Google Internal."
    },
    "internal7": {
     "type": "boolean",
     "description": "Google Internal."
    },
    "internal8": {
     "type": "array",
     "description": "Google Internal.",
     "items": {
      "type": "string"
     }
    },
    "inventories": {
     "type": "array",
     "description": "Inventories of product.",
     "items": {
      "type": "object",
      "properties": {
       "availability": {
        "type": "string",
        "description": "Availability of product inventory."
       },
       "channel": {
        "type": "string",
        "description": "Channel of product inventory (one of: online, local)."
       },
       "currency": {
        "type": "string",
        "description": "Currency of product inventory (an ISO 4217 alphabetic code)."
       },
       "distance": {
        "type": "number",
        "description": "Distance of product inventory.",
        "format": "float"
       },
       "distanceUnit": {
        "type": "string",
        "description": "Distance unit of product inventory."
       },
       "installmentMonths": {
        "type": "integer",
        "description": "Number of months for installment price.",
        "format": "int32"
       },
       "installmentPrice": {
        "type": "number",
        "description": "Installment price of product inventory.",
        "format": "float"
       },
       "originalPrice": {
        "type": "number",
        "description": "Original price of product inventory. Only returned for products that are on sale.",
        "format": "float"
       },
       "price": {
        "type": "number",
        "description": "Price of product inventory.",
        "format": "float"
       },
       "saleEndDate": {
        "type": "string",
        "description": "Sale end date.",
        "format": "date-time"
       },
       "salePrice": {
        "type": "number",
        "description": "Sale price of product inventory.",
        "format": "float"
       },
       "saleStartDate": {
        "type": "string",
        "description": "Sale start date.",
        "format": "date-time"
       },
       "shipping": {
        "type": "number",
        "description": "Shipping cost of product inventory.",
        "format": "float"
       },
       "storeId": {
        "type": "string",
        "description": "Store ID of product inventory."
       },
       "tax": {
        "type": "number",
        "description": "Tax of product inventory.",
        "format": "float"
       }
      }
     }
    },
    "language": {
     "type": "string",
     "description": "BCP 47 language tag of language of product."
    },
    "link": {
     "type": "string",
     "description": "Link to product."
    },
    "modificationTime": {
     "type": "string",
     "description": "RFC 3339 formatted modification time and date of product.",
     "format": "date-time"
    },
    "mpns": {
     "type": "array",
     "description": "List of all the product's MPNs.",
     "items": {
      "type": "string"
     }
    },
    "plusOne": {
     "type": "string",
     "description": "Code to add to the page to render the +1 content."
    },
    "providedId": {
     "type": "string",
     "description": "Merchant-provided id of product (available only with a cx source)."
    },
    "queryMatched": {
     "type": "boolean",
     "description": "Whether this product matched the user query. Only set for the variant offers (if any) attached to a product offer."
    },
    "score": {
     "type": "number",
     "description": "Google Internal",
     "format": "float"
    },
    "title": {
     "type": "string",
     "description": "Title of product."
    },
    "totalMatchingVariants": {
     "type": "integer",
     "description": "The number of variant offers returned that matched the query.",
     "format": "int32"
    },
    "variants": {
     "type": "array",
     "description": "A list of variant offers associated with this product.",
     "items": {
      "type": "object",
      "properties": {
       "variant": {
        "$ref": "ShoppingModelProductJsonV1",
        "description": "The detailed offer data for a particular variant offer."
       }
      }
     }
    }
   }
  },
  "ShoppingModelRecommendationsJsonV1": {
   "id": "ShoppingModelRecommendationsJsonV1",
   "type": "object",
   "properties": {
    "recommendationList": {
     "type": "array",
     "description": "List of recommendations.",
     "items": {
      "type": "object",
      "properties": {
       "product": {
        "$ref": "ShoppingModelProductJsonV1",
        "description": "Recommended product."
       }
      }
     }
    },
    "type": {
     "type": "string",
     "description": "Type of recommendation list (for offer-based recommendations, one of: all, purchaseToPurchase, visitToVisit, visitToPurchase, relatedItems; for category-based recommendations, one of: all, categoryMostVisited, categoryBestSeller)."
    }
   }
  }
 },
 "resources": {
  "products": {
   "methods": {
    "get": {
     "id": "shopping.products.get",
     "path": "{source}/products/{accountId}/{productIdType}/{productId}",
     "httpMethod": "GET",
     "description": "Returns a single product",
     "parameters": {
      "accountId": {
       "type": "integer",
       "description": "Merchant center account id",
       "required": true,
       "format": "uint32",
       "location": "path"
      },
      "attributeFilter": {
       "type": "string",
       "description": "Comma separated list of attributes to return",
       "location": "query"
      },
      "categories.enabled": {
       "type": "boolean",
       "description": "Whether to return category information",
       "location": "query"
      },
      "categories.include": {
       "type": "string",
       "description": "Category specification",
       "location": "query"
      },
      "categories.useGcsConfig": {
       "type": "boolean",
       "description": "This parameter is currently ignored",
       "location": "query"
      },
      "location": {
       "type": "string",
       "description": "Location used to determine tax and shipping",
       "location": "query"
      },
      "plusOne.enabled": {
       "type": "boolean",
       "description": "Whether to return +1 button code",
       "location": "query"
      },
      "plusOne.styles": {
       "type": "string",
       "description": "+1 button rendering styles",
       "location": "query"
      },
      "plusOne.useGcsConfig": {
       "type": "boolean",
       "description": "Whether to use +1 button styles configured in the GCS account",
       "location": "query"
      },
      "productId": {
       "type": "string",
       "description": "Id of product",
       "required": true,
       "location": "path"
      },
      "productIdType": {
       "type": "string",
       "description": "Type of productId",
       "required": true,
       "location": "path"
      },
      "recommendations.enabled": {
       "type": "boolean",
       "description": "Whether to return recommendation information",
       "location": "query"
      },
      "recommendations.include": {
       "type": "string",
       "description": "Recommendation specification",
       "location": "query"
      },
      "recommendations.useGcsConfig": {
       "type": "boolean",
       "description": "This parameter is currently ignored",
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "Query source",
       "required": true,
       "location": "path"
      },
      "taxonomy": {
       "type": "string",
       "description": "Merchant taxonomy",
       "location": "query"
      },
      "thumbnails": {
       "type": "string",
       "description": "Thumbnail specification",
       "location": "query"
      }
     },
     "parameterOrder": [
      "source",
      "accountId",
      "productIdType",
      "productId"
     ],
     "response": {
      "$ref": "Product"
     },
     "scopes": [
      "https://www.googleapis.com/auth/shoppingapi"
     ]
    },
    "list": {
     "id": "shopping.products.list",
     "path": "{source}/products",
     "httpMethod": "GET",
     "description": "Returns a list of products and content modules",
     "parameters": {
      "attributeFilter": {
       "type": "string",
       "description": "Comma separated list of attributes to return",
       "location": "query"
      },
      "availability": {
       "type": "string",
       "description": "Comma separated list of availabilities (outOfStock, limited, inStock, backOrder, preOrder, onDisplayToOrder) to return",
       "location": "query"
      },
      "boostBy": {
       "type": "string",
       "description": "Boosting specification",
       "location": "query"
      },
      "categories.enabled": {
       "type": "boolean",
       "description": "Whether to return category information",
       "location": "query"
      },
      "categories.include": {
       "type": "string",
       "description": "Category specification",
       "location": "query"
      },
      "categories.useGcsConfig": {
       "type": "boolean",
       "description": "This parameter is currently ignored",
       "location": "query"
      },
      "categoryRecommendations.category": {
       "type": "string",
       "description": "Category for which to retrieve recommendations",
       "location": "query"
      },
      "categoryRecommendations.enabled": {
       "type": "boolean",
       "description": "Whether to return category recommendation information",
       "location": "query"
      },
      "categoryRecommendations.include": {
       "type": "string",
       "description": "Category recommendation specification",
       "location": "query"
      },
      "categoryRecommendations.useGcsConfig": {
       "type": "boolean",
       "description": "This parameter is currently ignored",
       "location": "query"
      },
      "channels": {
       "type": "string",
       "description": "Channels specification",
       "location": "query"
      },
      "clickTracking": {
       "type": "boolean",
       "description": "Whether to add a click tracking parameter to offer URLs",
       "location": "query"
      },
      "country": {
       "type": "string",
       "description": "Country restriction (ISO 3166)",
       "location": "query"
      },
      "crowdBy": {
       "type": "string",
       "description": "Crowding specification",
       "location": "query"
      },
      "currency": {
       "type": "string",
       "description": "Currency restriction (ISO 4217)",
       "location": "query"
      },
      "facets.discover": {
       "type": "string",
       "description": "Facets to discover",
       "location": "query"
      },
      "facets.enabled": {
       "type": "boolean",
       "description": "Whether to return facet information",
       "location": "query"
      },
      "facets.include": {
       "type": "string",
       "description": "Facets to include (applies when useGcsConfig == false)",
       "location": "query"
      },
      "facets.useGcsConfig": {
       "type": "boolean",
       "description": "Whether to return facet information as configured in the GCS account",
       "location": "query"
      },
      "language": {
       "type": "string",
       "description": "Language restriction (BCP 47)",
       "location": "query"
      },
      "location": {
       "type": "string",
       "description": "Location used to determine tax and shipping",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "format": "uint32",
       "location": "query"
      },
      "maxVariants": {
       "type": "integer",
       "description": "Maximum number of variant results to return per result",
       "format": "int32",
       "location": "query"
      },
      "plusOne.enabled": {
       "type": "boolean",
       "description": "Whether to return +1 button code",
       "location": "query"
      },
      "plusOne.styles": {
       "type": "string",
       "description": "+1 button rendering styles",
       "location": "query"
      },
      "plusOne.useGcsConfig": {
       "type": "boolean",
       "description": "Whether to use +1 button styles configured in the GCS account",
       "location": "query"
      },
      "promotions.enabled": {
       "type": "boolean",
       "description": "Whether to return promotion information",
       "location": "query"
      },
      "promotions.useGcsConfig": {
       "type": "boolean",
       "description": "Whether to return promotion information as configured in the GCS account",
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "Search query",
       "location": "query"
      },
      "rankBy": {
       "type": "string",
       "description": "Ranking specification",
       "location": "query"
      },
      "redirects.enabled": {
       "type": "boolean",
       "description": "Whether to return redirect information",
       "location": "query"
      },
      "redirects.useGcsConfig": {
       "type": "boolean",
       "description": "Whether to return redirect information as configured in the GCS account",
       "location": "query"
      },
      "relatedQueries.enabled": {
       "type": "boolean",
       "description": "Whether to return related queries",
       "location": "query"
      },
      "relatedQueries.useGcsConfig": {
       "type": "boolean",
       "description": "This parameter is currently ignored",
       "location": "query"
      },
      "restrictBy": {
       "type": "string",
       "description": "Restriction specification",
       "location": "query"
      },
      "safe": {
       "type": "boolean",
       "description": "Whether safe search is enabled. Default: true",
       "location": "query"
      },
      "source": {
       "type": "string",
       "description": "Query source",
       "required": true,
       "location": "path"
      },
      "spelling.enabled": {
       "type": "boolean",
       "description": "Whether to return spelling suggestions",
       "location": "query"
      },
      "spelling.useGcsConfig": {
       "type": "boolean",
       "description": "This parameter is currently ignored",
       "location": "query"
      },
      "startIndex": {
       "type": "integer",
       "description": "Index (1-based) of first product to return",
       "format": "uint32",
       "location": "query"
      },
      "taxonomy": {
       "type": "string",
       "description": "Taxonomy name",
       "location": "query"
      },
      "thumbnails": {
       "type": "string",
       "description": "Image thumbnails specification",
       "location": "query"
      },
      "useCase": {
       "type": "string",
       "description": "One of CommerceSearchUseCase, ShoppingApiUseCase",
       "location": "query"
      }
     },
     "parameterOrder": [
      "source"
     ],
     "response": {
      "$ref": "Products"
     },
     "scopes": [
      "https://www.googleapis.com/auth/shoppingapi"
     ]
    }
   }
  }
 }
}
