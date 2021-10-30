-- |
module Notion.Pagination
  (
  )
  where

------------------------------------------------------------------------------

import Notion.Prelude

------------------------------------------------------------------------------

-- | Request parameters accepted by all the paginated endpoints.
--
-- For endpoints using the HTTP GET method, these parameters are accepted in
-- the request query string.
-- For endpoints using the HTTP POST method, these parameters are accepted in
-- the request body.
data PaginatedRequest = PaginatedRequest
  { -- | A cursor returned from a previous response, used to request the next
    -- page of results. Treat this as an opaque value.
    -- Default: undefined, which indicates to return results from the
    -- beginning of the list.
    _paginatedRequest_startCursor :: Maybe Text,
    -- | The number of items from the full list desired in the response.
    -- Default: 100
    -- Maximum: 100
    -- The response may contain fewer than this number of results.
    _paginatedRequest_pageSize :: Maybe Natural
  }

------------------------------------------------------------------------------

-- | Paginated endpoint response parametrized by the endpoint result.
data PaginatedResponse result = PaginatedResponse
  { -- | When the response includes the end of the list, `False`.
    -- Otherwise, `True`.
    _paginatedResponse_hasMore :: Bool,
    -- | Only available when `_paginatedResponse_hasMore` is `True`.
    -- Used to retrieve the next page of results by passing the value as the
    -- `_paginatedRequest_startCursor` parameter to the same endpoint.
    _paginatedResponse_nextCursor :: Text,
    -- | The page, or partial list, or results.
    _paginatedResponse_results :: [result],
    -- | Always "list".
    _paginatedResponse_object :: Text
  }
