{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "fusiontables:v1",
 "name": "fusiontables",
 "version": "v1",
 "revision": "20120831",
 "title": "Fusion Tables API",
 "description": "API for working with Fusion Tables data.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/search-16.gif",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "https://developers.google.com/fusiontables",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/fusiontables/v1/",
 "basePath": "/fusiontables/v1/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "fusiontables/v1/",
 "batchPath": "batch",
 "parameters": {
  "alt": {
   "type": "string",
   "description": "Data format for the response.",
   "default": "json",
   "enum": [
    "csv",
    "json"
   ],
   "enumDescriptions": [
    "Responses with Content-Type of text/csv",
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
    "https://www.googleapis.com/auth/fusiontables": {
     "description": "Manage your Fusion Tables"
    },
    "https://www.googleapis.com/auth/fusiontables.readonly": {
     "description": "View your Fusion Tables"
    }
   }
  }
 },
 "schemas": {
  "Bucket": {
   "id": "Bucket",
   "type": "object",
   "description": "Specifies the minimum and maximum values, the color, opacity, icon and weight of a bucket within a StyleSetting.",
   "properties": {
    "color": {
     "type": "string",
     "description": "Color of line or the interior of a polygon in #RRGGBB format."
    },
    "icon": {
     "type": "string",
     "description": "Icon name used for a point."
    },
    "max": {
     "type": "number",
     "description": "Maximum value in the selected column for a row to be styled according to the bucket color, opacity, icon, or weight.",
     "format": "double"
    },
    "min": {
     "type": "number",
     "description": "Minimum value in the selected column for a row to be styled according to the bucket color, opacity, icon, or weight.",
     "format": "double"
    },
    "opacity": {
     "type": "number",
     "description": "Opacity of the color: 0.0 (transparent) to 1.0 (opaque).",
     "format": "double"
    },
    "weight": {
     "type": "integer",
     "description": "Width of a line (in pixels).",
     "format": "int32"
    }
   }
  },
  "Column": {
   "id": "Column",
   "type": "object",
   "description": "Specifies the id, name and type of a column in a table.",
   "properties": {
    "baseColumn": {
     "type": "object",
     "description": "Optional identifier of the base column. If present, this column is derived from the specified base column.",
     "properties": {
      "columnId": {
       "type": "integer",
       "description": "The id of the column in the base table from which this column is derived.",
       "format": "int32"
      },
      "tableIndex": {
       "type": "integer",
       "description": "Offset to the entry in the list of base tables in the table definition.",
       "format": "int32"
      }
     }
    },
    "columnId": {
     "type": "integer",
     "description": "Identifier for the column.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Type name: a template for an individual column.",
     "default": "fusiontables#column"
    },
    "name": {
     "type": "string",
     "description": "Required name of the column.",
     "annotations": {
      "required": [
       "fusiontables.column.insert"
      ]
     }
    },
    "type": {
     "type": "string",
     "description": "Required type of the column.",
     "annotations": {
      "required": [
       "fusiontables.column.insert"
      ]
     }
    }
   }
  },
  "ColumnList": {
   "id": "ColumnList",
   "type": "object",
   "description": "Represents a list of columns in a table.",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of all requested columns.",
     "items": {
      "$ref": "Column"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type name: a list of all tables.",
     "default": "fusiontables#columnList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token used to access the next page of this result. No token is displayed if there are no more tokens left."
    },
    "totalItems": {
     "type": "integer",
     "description": "Total number of columns for the table.",
     "format": "int32"
    }
   }
  },
  "Geometry": {
   "id": "Geometry",
   "type": "object",
   "description": "Represents a Geometry object.",
   "properties": {
    "geometries": {
     "type": "array",
     "description": "The list of geometries in this geometry collection.",
     "items": {
      "type": "any"
     }
    },
    "geometry": {
     "type": "any"
    },
    "type": {
     "type": "string",
     "description": "Type: A collection of geometries.",
     "default": "GeometryCollection"
    }
   }
  },
  "Import": {
   "id": "Import",
   "type": "object",
   "description": "Represents an import request.",
   "properties": {
    "kind": {
     "type": "string",
     "description": "Type name: a template for an import request.",
     "default": "fusiontables#import"
    },
    "numRowsReceived": {
     "type": "string",
     "description": "The number of rows received from the import request.",
     "format": "int64"
    }
   }
  },
  "Line": {
   "id": "Line",
   "type": "object",
   "description": "Represents a line geometry.",
   "properties": {
    "coordinates": {
     "type": "array",
     "description": "The coordinates that define the line.",
     "items": {
      "type": "array",
      "items": {
       "type": "number",
       "format": "double"
      }
     }
    },
    "type": {
     "type": "string",
     "description": "Type: A line geometry.",
     "default": "LineString"
    }
   }
  },
  "LineStyle": {
   "id": "LineStyle",
   "type": "object",
   "description": "Represents a LineStyle within a StyleSetting",
   "properties": {
    "strokeColor": {
     "type": "string",
     "description": "Color of the line in #RRGGBB format."
    },
    "strokeColorStyler": {
     "$ref": "StyleFunction",
     "description": "Column-value, gradient or buckets styler that is used to determine the line color and opacity."
    },
    "strokeOpacity": {
     "type": "number",
     "description": "Opacity of the line : 0.0 (transparent) to 1.0 (opaque).",
     "format": "double"
    },
    "strokeWeight": {
     "type": "integer",
     "description": "Width of the line in pixels.",
     "format": "int32"
    },
    "strokeWeightStyler": {
     "$ref": "StyleFunction",
     "description": "Column-value or bucket styler that is used to determine the width of the line."
    }
   }
  },
  "Point": {
   "id": "Point",
   "type": "object",
   "description": "Represents a point object.",
   "properties": {
    "coordinates": {
     "type": "array",
     "description": "The coordinates that define the point.",
     "items": {
      "type": "number",
      "format": "double"
     }
    },
    "type": {
     "type": "string",
     "description": "Point: A point geometry.",
     "default": "Point"
    }
   }
  },
  "PointStyle": {
   "id": "PointStyle",
   "type": "object",
   "description": "Represents a PointStyle within a StyleSetting",
   "properties": {
    "iconName": {
     "type": "string",
     "description": "Name of the icon. Use values defined in http://www.google.com/fusiontables/DataSource?dsrcid=308519"
    },
    "iconStyler": {
     "$ref": "StyleFunction",
     "description": "Column or a bucket value from which the icon name is to be determined."
    }
   }
  },
  "Polygon": {
   "id": "Polygon",
   "type": "object",
   "description": "Represents a polygon object.",
   "properties": {
    "coordinates": {
     "type": "array",
     "description": "The coordinates that define the polygon.",
     "items": {
      "type": "array",
      "items": {
       "type": "array",
       "items": {
        "type": "number",
        "format": "double"
       }
      }
     }
    },
    "type": {
     "type": "string",
     "description": "Type: A polygon geometry.",
     "default": "Polygon"
    }
   }
  },
  "PolygonStyle": {
   "id": "PolygonStyle",
   "type": "object",
   "description": "Represents a PolygonStyle within a StyleSetting",
   "properties": {
    "fillColor": {
     "type": "string",
     "description": "Color of the interior of the polygon in #RRGGBB format."
    },
    "fillColorStyler": {
     "$ref": "StyleFunction",
     "description": "Column-value, gradient, or bucket styler that is used to determine the interior color and opacity of the polygon."
    },
    "fillOpacity": {
     "type": "number",
     "description": "Opacity of the interior of the polygon: 0.0 (transparent) to 1.0 (opaque).",
     "format": "double"
    },
    "strokeColor": {
     "type": "string",
     "description": "Color of the polygon border in #RRGGBB format."
    },
    "strokeColorStyler": {
     "$ref": "StyleFunction",
     "description": "Column-value, gradient or buckets styler that is used to determine the border color and opacity."
    },
    "strokeOpacity": {
     "type": "number",
     "description": "Opacity of the polygon border: 0.0 (transparent) to 1.0 (opaque).",
     "format": "double"
    },
    "strokeWeight": {
     "type": "integer",
     "description": "Width of the polyon border in pixels.",
     "format": "int32"
    },
    "strokeWeightStyler": {
     "$ref": "StyleFunction",
     "description": "Column-value or bucket styler that is used to determine the width of the polygon border."
    }
   }
  },
  "Sqlresponse": {
   "id": "Sqlresponse",
   "type": "object",
   "description": "Represents a response to an sql statement.",
   "properties": {
    "columns": {
     "type": "array",
     "description": "Columns in the table.",
     "items": {
      "type": "string"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type name: a template for an individual table.",
     "default": "fusiontables#sqlresponse"
    },
    "rows": {
     "type": "array",
     "description": "The rows in the table. For each cell we print out whatever cell value (e.g., numeric, string) exists. Thus it is important that each cell contains only one value.",
     "items": {
      "type": "array",
      "items": {
       "type": "any"
      }
     }
    }
   }
  },
  "StyleFunction": {
   "id": "StyleFunction",
   "type": "object",
   "description": "Represents a StyleFunction within a StyleSetting",
   "properties": {
    "buckets": {
     "type": "array",
     "description": "Bucket function that assigns a style based on the range a column value falls into.",
     "items": {
      "$ref": "Bucket"
     }
    },
    "columnName": {
     "type": "string",
     "description": "Name of the column whose value is used in the style.",
     "annotations": {
      "required": [
       "fusiontables.style.insert"
      ]
     }
    },
    "gradient": {
     "type": "object",
     "description": "Gradient function that interpolates a range of colors based on column value.",
     "properties": {
      "colors": {
       "type": "array",
       "description": "Array with two or more colors.",
       "items": {
        "type": "object",
        "properties": {
         "color": {
          "type": "string",
          "description": "Color in #RRGGBB format."
         },
         "opacity": {
          "type": "number",
          "description": "Opacity of the color: 0.0 (transparent) to 1.0 (opaque).",
          "format": "double"
         }
        }
       }
      },
      "max": {
       "type": "number",
       "description": "Higher-end of the interpolation range: rows with this value will be assigned to colors[n-1].",
       "format": "double"
      },
      "min": {
       "type": "number",
       "description": "Lower-end of the interpolation range: rows with this value will be assigned to colors[0].",
       "format": "double"
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "Stylers can be one of three kinds: \"fusiontables#fromColumn\" if the column value is to be used as is, i.e., the column values can have colors in #RRGGBBAA format or integer line widths or icon names; \"fusiontables#gradient\" if the styling of the row is to be based on applying the gradient function on the column value; or \"fusiontables#buckets\" if the styling is to based on the bucket into which the the column value falls."
    }
   }
  },
  "StyleSetting": {
   "id": "StyleSetting",
   "type": "object",
   "description": "Represents a complete StyleSettings object. The primary key is a combination of the tableId and a styleId.",
   "properties": {
    "isDefaultForTable": {
     "type": "boolean",
     "description": "Is this the default style for the table."
    },
    "kind": {
     "type": "string",
     "description": "Type name: an individual style setting. A StyleSetting contains the style defintions for points, lines, and polygons in a table. Since a table can have any one or all of them, a style definition can have point, line and polygon style definitions.",
     "default": "fusiontables#styleSetting"
    },
    "markerOptions": {
     "$ref": "PointStyle",
     "description": "Style definition for points in the table."
    },
    "name": {
     "type": "string",
     "description": "Optional name for the style setting."
    },
    "polygonOptions": {
     "$ref": "PolygonStyle",
     "description": "Style definition for polygons in the table."
    },
    "polylineOptions": {
     "$ref": "LineStyle",
     "description": "Style definition for lines in the table."
    },
    "styleId": {
     "type": "integer",
     "description": "Identifier for the style setting (unique only within tables).",
     "format": "int32"
    },
    "tableId": {
     "type": "string",
     "description": "Identifier for the table."
    }
   }
  },
  "StyleSettingList": {
   "id": "StyleSettingList",
   "type": "object",
   "description": "Represents a list of styles for a given table.",
   "properties": {
    "items": {
     "type": "array",
     "description": "All requested style settings.",
     "items": {
      "$ref": "StyleSetting"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type name: in this case, a list of style settings.",
     "default": "fusiontables#styleSettingList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token used to access the next page of this result. No token is displayed if there are no more styles left."
    },
    "totalItems": {
     "type": "integer",
     "description": "Total number of styles for the table.",
     "format": "int32"
    }
   }
  },
  "Table": {
   "id": "Table",
   "type": "object",
   "description": "Represents a table. Specifies the name, whether it is exportable, description, attribution, and attribution link.",
   "properties": {
    "attribution": {
     "type": "string",
     "description": "Optional attribution assigned to the table."
    },
    "attributionLink": {
     "type": "string",
     "description": "Optional link for attribution."
    },
    "baseTableIds": {
     "type": "array",
     "description": "Optional base table identifier if this table is a view or merged table.",
     "items": {
      "type": "string"
     }
    },
    "columns": {
     "type": "array",
     "description": "Columns in the table.",
     "items": {
      "$ref": "Column"
     },
     "annotations": {
      "required": [
       "fusiontables.table.insert",
       "fusiontables.table.update"
      ]
     }
    },
    "description": {
     "type": "string",
     "description": "Optional description assigned to the table."
    },
    "isExportable": {
     "type": "boolean",
     "description": "Variable for whether table is exportable.",
     "annotations": {
      "required": [
       "fusiontables.table.insert",
       "fusiontables.table.update"
      ]
     }
    },
    "kind": {
     "type": "string",
     "description": "Type name: a template for an individual table.",
     "default": "fusiontables#table"
    },
    "name": {
     "type": "string",
     "description": "Name assigned to a table.",
     "annotations": {
      "required": [
       "fusiontables.table.insert",
       "fusiontables.table.update"
      ]
     }
    },
    "sql": {
     "type": "string",
     "description": "Optional sql that encodes the table definition for derived tables."
    },
    "tableId": {
     "type": "string",
     "description": "Encrypted unique alphanumeric identifier for the table."
    }
   }
  },
  "TableList": {
   "id": "TableList",
   "type": "object",
   "description": "Represents a list of tables.",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of all requested tables.",
     "items": {
      "$ref": "Table"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type name: a list of all tables.",
     "default": "fusiontables#tableList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token used to access the next page of this result. No token is displayed if there are no more tokens left."
    }
   }
  },
  "Template": {
   "id": "Template",
   "type": "object",
   "description": "Represents the contents of InfoWindow templates.",
   "properties": {
    "automaticColumnNames": {
     "type": "array",
     "description": "List of columns from which the template is to be automatically constructed. Only one of body or automaticColumns can be specified.",
     "items": {
      "type": "string"
     }
    },
    "body": {
     "type": "string",
     "description": "Body of the template. It contains HTML with {column_name} to insert values from a particular column. The body is sanitized to remove certain tags, e.g., script. Only one of body or automaticColumns can be specified."
    },
    "isDefaultForTable": {
     "type": "boolean",
     "description": "Is this the default template for the table."
    },
    "kind": {
     "type": "string",
     "description": "Type name: a template for the info window contents. The template can either include an HTML body or a list of columns from which the template is computed automatically.",
     "default": "fusiontables#template"
    },
    "name": {
     "type": "string",
     "description": "Optional name assigned to a template."
    },
    "tableId": {
     "type": "string",
     "description": "Identifier for the table for which the template is defined."
    },
    "templateId": {
     "type": "integer",
     "description": "Identifier for the template, unique within the context of a particular table.",
     "format": "int32"
    }
   }
  },
  "TemplateList": {
   "id": "TemplateList",
   "type": "object",
   "description": "Represents a list of templates for a given table.",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of all requested templates.",
     "items": {
      "$ref": "Template"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type name: a list of all templates.",
     "default": "fusiontables#templateList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token used to access the next page of this result. No token is displayed if there are no more tokens left."
    },
    "totalItems": {
     "type": "integer",
     "description": "Total number of templates for the table.",
     "format": "int32"
    }
   }
  }
 },
 "resources": {
  "column": {
   "methods": {
    "delete": {
     "id": "fusiontables.column.delete",
     "path": "tables/{tableId}/columns/{columnId}",
     "httpMethod": "DELETE",
     "description": "Deletes the column.",
     "parameters": {
      "columnId": {
       "type": "string",
       "description": "Name or identifier for the column being deleted.",
       "required": true,
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table from which the column is being deleted.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "columnId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "get": {
     "id": "fusiontables.column.get",
     "path": "tables/{tableId}/columns/{columnId}",
     "httpMethod": "GET",
     "description": "Retrieves a specific column by its id.",
     "parameters": {
      "columnId": {
       "type": "string",
       "description": "Name or identifier for the column that is being requested.",
       "required": true,
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table to which the column belongs.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "columnId"
     ],
     "response": {
      "$ref": "Column"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "insert": {
     "id": "fusiontables.column.insert",
     "path": "tables/{tableId}/columns",
     "httpMethod": "POST",
     "description": "Adds a new column to the table.",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "Table for which a new column is being added.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "request": {
      "$ref": "Column"
     },
     "response": {
      "$ref": "Column"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "list": {
     "id": "fusiontables.column.list",
     "path": "tables/{tableId}/columns",
     "httpMethod": "GET",
     "description": "Retrieves a list of columns.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of columns to return. Optional. Default is 5.",
       "format": "uint32",
       "minimum": "0",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Continuation token specifying which result page to return. Optional.",
       "location": "query"
      },
      "tableId": {
       "type": "string",
       "description": "Table whose columns are being listed.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "response": {
      "$ref": "ColumnList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "patch": {
     "id": "fusiontables.column.patch",
     "path": "tables/{tableId}/columns/{columnId}",
     "httpMethod": "PATCH",
     "description": "Updates the name or type of an existing column. This method supports patch semantics.",
     "parameters": {
      "columnId": {
       "type": "string",
       "description": "Name or identifier for the column that is being updated.",
       "required": true,
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table for which the column is being updated.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "columnId"
     ],
     "request": {
      "$ref": "Column"
     },
     "response": {
      "$ref": "Column"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "update": {
     "id": "fusiontables.column.update",
     "path": "tables/{tableId}/columns/{columnId}",
     "httpMethod": "PUT",
     "description": "Updates the name or type of an existing column.",
     "parameters": {
      "columnId": {
       "type": "string",
       "description": "Name or identifier for the column that is being updated.",
       "required": true,
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table for which the column is being updated.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "columnId"
     ],
     "request": {
      "$ref": "Column"
     },
     "response": {
      "$ref": "Column"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    }
   }
  },
  "import": {
   "methods": {
    "insert": {
     "id": "fusiontables.import.insert",
     "path": "tables/{tableId}/import",
     "httpMethod": "POST",
     "description": "Import more rows into a table.",
     "parameters": {
      "delimiter": {
       "type": "string",
       "description": "The delimiter used to separate cell values. Default is ','.",
       "location": "query"
      },
      "encoding": {
       "type": "string",
       "description": "The encoding of the content. Default is UTF-8.",
       "location": "query"
      },
      "endLine": {
       "type": "string",
       "description": "The index of the last line from which to start importing, exclusive. Thus, the number of imported lines is endLine - startLine. If this parameter is not provided, the file will be imported until the last line of the file. If endLine is negative, then it is equivalent to N + endLine, where N is the number of lines in the file.",
       "format": "int64",
       "location": "query"
      },
      "isStrict": {
       "type": "boolean",
       "description": "Whether the CSV will be parsed strictly or not. Default is true.",
       "location": "query"
      },
      "startLine": {
       "type": "string",
       "description": "The index of the first line from which to start importing, inclusive. Default is 0.",
       "format": "int64",
       "location": "query"
      },
      "tableId": {
       "type": "string",
       "description": "The table into which new rows are being imported.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "response": {
      "$ref": "Import"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ],
     "supportsMediaUpload": true,
     "mediaUpload": {
      "accept": [
       "application/octet-stream"
      ],
      "maxSize": "100MB",
      "protocols": {
       "simple": {
        "multipart": true,
        "path": "/upload/fusiontables/v1/tables/{tableId}/import"
       },
       "resumable": {
        "multipart": true,
        "path": "/resumable/upload/fusiontables/v1/tables/{tableId}/import"
       }
      }
     }
    }
   }
  },
  "query": {
   "methods": {
    "sql": {
     "id": "fusiontables.query.sql",
     "path": "query",
     "httpMethod": "POST",
     "description": "Executes an SQL SELECT/INSERT/UPDATE/DELETE/SHOW/DESCRIBE/CREATE statement.",
     "parameters": {
      "hdrs": {
       "type": "boolean",
       "description": "Should column names be included (in the first row)?. Default is true.",
       "location": "query"
      },
      "sql": {
       "type": "string",
       "description": "An SQL SELECT/SHOW/DESCRIBE/INSERT/UPDATE/DELETE/CREATE statement.",
       "required": true,
       "location": "query"
      },
      "typed": {
       "type": "boolean",
       "description": "Should typed values be returned in the (JSON) response -- numbers for numeric values and parsed geometries for KML values? Default is true.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "sql"
     ],
     "response": {
      "$ref": "Sqlresponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "sqlGet": {
     "id": "fusiontables.query.sqlGet",
     "path": "query",
     "httpMethod": "GET",
     "description": "Executes an SQL SELECT/SHOW/DESCRIBE statement.",
     "parameters": {
      "hdrs": {
       "type": "boolean",
       "description": "Should column names be included (in the first row)?. Default is true.",
       "location": "query"
      },
      "sql": {
       "type": "string",
       "description": "An SQL SELECT/SHOW/DESCRIBE statement.",
       "required": true,
       "location": "query"
      },
      "typed": {
       "type": "boolean",
       "description": "Should typed values be returned in the (JSON) response -- numbers for numeric values and parsed geometries for KML values? Default is true.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "sql"
     ],
     "response": {
      "$ref": "Sqlresponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    }
   }
  },
  "style": {
   "methods": {
    "delete": {
     "id": "fusiontables.style.delete",
     "path": "tables/{tableId}/styles/{styleId}",
     "httpMethod": "DELETE",
     "description": "Deletes a style.",
     "parameters": {
      "styleId": {
       "type": "integer",
       "description": "Identifier (within a table) for the style being deleted",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table from which the style is being deleted",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "styleId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "get": {
     "id": "fusiontables.style.get",
     "path": "tables/{tableId}/styles/{styleId}",
     "httpMethod": "GET",
     "description": "Gets a specific style.",
     "parameters": {
      "styleId": {
       "type": "integer",
       "description": "Identifier (integer) for a specific style in a table",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table to which the requested style belongs",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "styleId"
     ],
     "response": {
      "$ref": "StyleSetting"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "insert": {
     "id": "fusiontables.style.insert",
     "path": "tables/{tableId}/styles",
     "httpMethod": "POST",
     "description": "Adds a new style for the table.",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "Table for which a new style is being added",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "request": {
      "$ref": "StyleSetting"
     },
     "response": {
      "$ref": "StyleSetting"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "list": {
     "id": "fusiontables.style.list",
     "path": "tables/{tableId}/styles",
     "httpMethod": "GET",
     "description": "Retrieves a list of styles.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of styles to return. Optional. Default is 5.",
       "format": "uint32",
       "minimum": "0",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Continuation token specifying which result page to return. Optional.",
       "location": "query"
      },
      "tableId": {
       "type": "string",
       "description": "Table whose styles are being listed",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "response": {
      "$ref": "StyleSettingList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "patch": {
     "id": "fusiontables.style.patch",
     "path": "tables/{tableId}/styles/{styleId}",
     "httpMethod": "PATCH",
     "description": "Updates an existing style. This method supports patch semantics.",
     "parameters": {
      "styleId": {
       "type": "integer",
       "description": "Identifier (within a table) for the style being updated.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table whose style is being updated.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "styleId"
     ],
     "request": {
      "$ref": "StyleSetting"
     },
     "response": {
      "$ref": "StyleSetting"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "update": {
     "id": "fusiontables.style.update",
     "path": "tables/{tableId}/styles/{styleId}",
     "httpMethod": "PUT",
     "description": "Updates an existing style.",
     "parameters": {
      "styleId": {
       "type": "integer",
       "description": "Identifier (within a table) for the style being updated.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table whose style is being updated.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "styleId"
     ],
     "request": {
      "$ref": "StyleSetting"
     },
     "response": {
      "$ref": "StyleSetting"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    }
   }
  },
  "table": {
   "methods": {
    "copy": {
     "id": "fusiontables.table.copy",
     "path": "tables/{tableId}/copy",
     "httpMethod": "POST",
     "description": "Copies a table.",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "ID of the table that is being copied.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "delete": {
     "id": "fusiontables.table.delete",
     "path": "tables/{tableId}",
     "httpMethod": "DELETE",
     "description": "Deletes a table.",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "ID of the table that is being deleted.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "get": {
     "id": "fusiontables.table.get",
     "path": "tables/{tableId}",
     "httpMethod": "GET",
     "description": "Retrieves a specific table by its id.",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "Identifier(ID) for the table being requested.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "insert": {
     "id": "fusiontables.table.insert",
     "path": "tables",
     "httpMethod": "POST",
     "description": "Creates a new table.",
     "request": {
      "$ref": "Table"
     },
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "list": {
     "id": "fusiontables.table.list",
     "path": "tables",
     "httpMethod": "GET",
     "description": "Retrieves a list of tables a user owns.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of styles to return. Optional. Default is 5.",
       "format": "uint32",
       "minimum": "0",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Continuation token specifying which result page to return. Optional.",
       "location": "query"
      }
     },
     "response": {
      "$ref": "TableList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "patch": {
     "id": "fusiontables.table.patch",
     "path": "tables/{tableId}",
     "httpMethod": "PATCH",
     "description": "Updates an existing table. Unless explicitly requested, only the name, description, and attribution will be updated. This method supports patch semantics.",
     "parameters": {
      "replaceViewDefinition": {
       "type": "boolean",
       "description": "Should the view definition also be updated? The specified view definition replaces the existing one. Only a view can be updated with a new definition.",
       "location": "query"
      },
      "tableId": {
       "type": "string",
       "description": "ID of the table that is being updated.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "request": {
      "$ref": "Table"
     },
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "update": {
     "id": "fusiontables.table.update",
     "path": "tables/{tableId}",
     "httpMethod": "PUT",
     "description": "Updates an existing table. Unless explicitly requested, only the name, description, and attribution will be updated.",
     "parameters": {
      "replaceViewDefinition": {
       "type": "boolean",
       "description": "Should the view definition also be updated? The specified view definition replaces the existing one. Only a view can be updated with a new definition.",
       "location": "query"
      },
      "tableId": {
       "type": "string",
       "description": "ID of the table that is being updated.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "request": {
      "$ref": "Table"
     },
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    }
   }
  },
  "template": {
   "methods": {
    "delete": {
     "id": "fusiontables.template.delete",
     "path": "tables/{tableId}/templates/{templateId}",
     "httpMethod": "DELETE",
     "description": "Deletes a template",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "Table from which the template is being deleted",
       "required": true,
       "location": "path"
      },
      "templateId": {
       "type": "integer",
       "description": "Identifier for the template which is being deleted",
       "required": true,
       "format": "int32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "templateId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "get": {
     "id": "fusiontables.template.get",
     "path": "tables/{tableId}/templates/{templateId}",
     "httpMethod": "GET",
     "description": "Retrieves a specific template by its id",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "Table to which the template belongs",
       "required": true,
       "location": "path"
      },
      "templateId": {
       "type": "integer",
       "description": "Identifier for the template that is being requested",
       "required": true,
       "format": "int32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "templateId"
     ],
     "response": {
      "$ref": "Template"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "insert": {
     "id": "fusiontables.template.insert",
     "path": "tables/{tableId}/templates",
     "httpMethod": "POST",
     "description": "Creates a new template for the table.",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "Table for which a new template is being created",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "request": {
      "$ref": "Template"
     },
     "response": {
      "$ref": "Template"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "list": {
     "id": "fusiontables.template.list",
     "path": "tables/{tableId}/templates",
     "httpMethod": "GET",
     "description": "Retrieves a list of templates.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of templates to return. Optional. Default is 5.",
       "format": "uint32",
       "minimum": "0",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Continuation token specifying which results page to return. Optional.",
       "location": "query"
      },
      "tableId": {
       "type": "string",
       "description": "Identifier for the table whose templates are being requested",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId"
     ],
     "response": {
      "$ref": "TemplateList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables",
      "https://www.googleapis.com/auth/fusiontables.readonly"
     ]
    },
    "patch": {
     "id": "fusiontables.template.patch",
     "path": "tables/{tableId}/templates/{templateId}",
     "httpMethod": "PATCH",
     "description": "Updates an existing template. This method supports patch semantics.",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "Table to which the updated template belongs",
       "required": true,
       "location": "path"
      },
      "templateId": {
       "type": "integer",
       "description": "Identifier for the template that is being updated",
       "required": true,
       "format": "int32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "templateId"
     ],
     "request": {
      "$ref": "Template"
     },
     "response": {
      "$ref": "Template"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    },
    "update": {
     "id": "fusiontables.template.update",
     "path": "tables/{tableId}/templates/{templateId}",
     "httpMethod": "PUT",
     "description": "Updates an existing template",
     "parameters": {
      "tableId": {
       "type": "string",
       "description": "Table to which the updated template belongs",
       "required": true,
       "location": "path"
      },
      "templateId": {
       "type": "integer",
       "description": "Identifier for the template that is being updated",
       "required": true,
       "format": "int32",
       "location": "path"
      }
     },
     "parameterOrder": [
      "tableId",
      "templateId"
     ],
     "request": {
      "$ref": "Template"
     },
     "response": {
      "$ref": "Template"
     },
     "scopes": [
      "https://www.googleapis.com/auth/fusiontables"
     ]
    }
   }
  }
 }
}
