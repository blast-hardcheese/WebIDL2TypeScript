module Tizen {
  enum FilterMatchFlag { "EXACTLY", "FULLSTRING", "CONTAINS", "STARTSWITH", "ENDSWITH", "EXISTS" };

  enum SortModeOrder { "ASC", "DESC" };

  enum CompositeFilterType { "UNION", "INTERSECTION" };

  [NoInterfaceObject] interface TizenObject {
    readonly attribute Tizen tizen;
  };
  Window implements TizenObject;

  [NoInterfaceObject] interface Tizen {
  };

  [NoInterfaceObject] interface AbstractFilter {
  };

  [Constructor(DOMString attributeName, optional FilterMatchFlag? matchFlag, optional any matchValue)]
  interface AttributeFilter : AbstractFilter {
    attribute DOMString attributeName;

    attribute FilterMatchFlag matchFlag;

    attribute any matchValue;
  };

  [Constructor(DOMString attributeName, optional any initialValue, optional any endValue)]
  interface AttributeRangeFilter : AbstractFilter {
    attribute DOMString attributeName;

    attribute any initialValue;

    attribute any endValue;
  };

  [Constructor(CompositeFilterType type, optional AbstractFilter[]? filters)]
  interface CompositeFilter : AbstractFilter {

    attribute CompositeFilterType type;

    attribute AbstractFilter[] filters;
  };

  [Constructor(DOMString attributeName, optional SortModeOrder? order)]
  interface SortMode {
    attribute DOMString attributeName;

    attribute SortModeOrder order;
  };

  [Constructor(double latitude, double longitude)]
  interface SimpleCoordinates {
    attribute double latitude;

    attribute double longitude;
  };

  [NoInterfaceObject]
  interface WebAPIException {
    readonly attribute unsigned short code;

    readonly attribute DOMString name;

    readonly attribute DOMString message;

    const unsigned short INDEX_SIZE_ERR = 1;
    const unsigned short DOMSTRING_SIZE_ERR = 2; 
    const unsigned short HIERARCHY_REQUEST_ERR = 3;
    const unsigned short WRONG_DOCUMENT_ERR = 4;
    const unsigned short INVALID_CHARACTER_ERR = 5;
    const unsigned short NO_DATA_ALLOWED_ERR = 6; 
    const unsigned short NO_MODIFICATION_ALLOWED_ERR = 7;
    const unsigned short NOT_FOUND_ERR = 8;
    const unsigned short NOT_SUPPORTED_ERR = 9;
    const unsigned short INUSE_ATTRIBUTE_ERR = 10; 
    const unsigned short INVALID_STATE_ERR = 11;
    const unsigned short SYNTAX_ERR = 12;
    const unsigned short INVALID_MODIFICATION_ERR = 13;
    const unsigned short NAMESPACE_ERR = 14;
    const unsigned short INVALID_ACCESS_ERR = 15;
    const unsigned short VALIDATION_ERR = 16; 
    const unsigned short TYPE_MISMATCH_ERR = 17;
    const unsigned short SECURITY_ERR = 18;
    const unsigned short NETWORK_ERR = 19;
    const unsigned short ABORT_ERR = 20;
    const unsigned short URL_MISMATCH_ERR = 21;
    const unsigned short QUOTA_EXCEEDED_ERR = 22;
    const unsigned short TIMEOUT_ERR = 23;
    const unsigned short INVALID_NODE_TYPE_ERR = 24;
    const unsigned short DATA_CLONE_ERR = 25;
  };

  [NoInterfaceObject]
  interface WebAPIError {
    readonly attribute unsigned short code;

    readonly attribute DOMString name;

    readonly attribute DOMString message;
  };

  [Callback=FunctionOnly, NoInterfaceObject]
  interface SuccessCallback {
    void onsuccess ();
  };

  [Callback=FunctionOnly, NoInterfaceObject]
  interface ErrorCallback {

    void onerror (WebAPIError error);
  };
};
