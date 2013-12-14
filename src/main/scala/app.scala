import util.parsing.combinator.RegexParsers

object DocParser extends RegexParsers {
  val identifier = "[a-zA-Z][a-zA-Z0-9]*".r

  val jsString = "DOMString" ^^ { _ => "string" }
  val jsNumber = "long" | "int" ^^ { _ => "number" }
  val jsPassthrough = "void" | "boolean" | "Date"

  val builtinType = jsString | jsNumber | jsPassthrough

  val tizenType =
    "ApplicationContextArraySuccessCallback" |
    "ApplicationContextId" |
    "ApplicationContext" |
    "ApplicationControlDataArrayReplyCallback" |
    "ApplicationControlData" |
    "ApplicationControl" |
    "ApplicationCertificate" |
    "ApplicationInformationEventCallback" |
    "ApplicationInformationArraySuccessCallback" |
    "ApplicationInformation" |
    "ApplicationId" |
    "ApplicationMetaData" |
    "ApplicationManager" |
    "Application" |
    "PackageId" |
    "RequestedApplicationControl" |
    "ErrorCallback" |
    "FindAppControlSuccessCallback" |
    "SuccessCallback"

  val jsType = (builtinType | tizenType) ~ opt("[]") ~ opt("?")
  val optionalType = "optional" ~ jsType

  val typedef = "typedef" ~> builtinType ~ tizenType ^^ { case t ~ i => s"// $i: $t" }
  val moduleLine = ( typedef ) <~ ";"

  val attribute = "readonly"
  val attributes = opt(attribute) ~ "attribute"
  val implementation = identifier ~ "implements" ~ identifier ~ ";"


  val packageLine = rep(packageMethod | packageProperty)
  val packageProperty = attributes ~> jsType ~ identifier <~ ";" ^^ { case t ~ i => s"$i: $t" }
  val packageMethodArgs = repsep((jsType | optionalType) ~ identifier, ",")
  val packageMethod = jsType ~ identifier ~ "(" ~ packageMethodArgs ~ ")" ~ ";"

  val module = "module" ~ identifier ~ "{" ~ rep(moduleLine | interface | implementation) ~ "};"
  val interface = "[" ~ interfaceBracket ~ "]" ~ "interface" ~ identifier ~ "{" ~ packageLine ~ "}" ~ ";"
  val interfaceBracketTokens = "NoInterfaceObject"
  val interfaceBracketConstructor = "Constructor(" ~ packageMethodArgs ~ ")"
  val interfaceCallback = "Callback" ~ opt("=FunctionOnly")
  val interfaceBracket = repsep(interfaceBracketTokens | interfaceBracketConstructor | interfaceCallback, ",")

  def parseLine[T](parser: Parser[T], input: String) = parseAll(parser, input)
  def apply(input: String) = parseAll(module, input)
}

object App extends App {

val typedef = "typedef DOMString ApplicationId;"
val optional = "optional SuccessCallback? successCallback"

val method = """
    void kill(ApplicationContextId contextId,
              optional SuccessCallback? successCallback,
              optional ErrorCallback? errorCallback);
"""

val packageArgs = """
  ApplicationContextId contextId,
  optional SuccessCallback? successCallback,
  optional ErrorCallback? errorCallback
"""

val s = """
module Application {

  typedef DOMString ApplicationId;

  typedef DOMString ApplicationContextId;

  [NoInterfaceObject] interface ApplicationManagerObject {
    readonly attribute ApplicationManager application;
  };
  Tizen implements ApplicationManagerObject;

  [NoInterfaceObject] interface ApplicationManager {

    Application getCurrentApplication();

    void kill(ApplicationContextId contextId,
              optional SuccessCallback? successCallback,
              optional ErrorCallback? errorCallback);

    void launch(ApplicationId id,
                optional SuccessCallback? successCallback,
                optional ErrorCallback? errorCallback);

    void launchAppControl(ApplicationControl appControl,
                          optional ApplicationId? id,
                          optional SuccessCallback? successCallback,
                          optional ErrorCallback? errorCallback,
                          optional ApplicationControlDataArrayReplyCallback? replyCallback);

    void findAppControl(ApplicationControl appControl,
                        FindAppControlSuccessCallback successCallback,
                        optional ErrorCallback? errorCallback);

    void getAppsContext(ApplicationContextArraySuccessCallback successCallback,
                        optional ErrorCallback? errorCallback);

    ApplicationContext getAppContext(optional ApplicationContextId? contextId);

    void getAppsInfo(ApplicationInformationArraySuccessCallback successCallback,
                     optional ErrorCallback? errorCallback);

    ApplicationInformation getAppInfo(optional ApplicationId? id);

    ApplicationCertificate[] getAppCerts(optional ApplicationId? id);

    DOMString getAppSharedURI(optional ApplicationId? id);

    ApplicationMetaData[] getAppMetaData(optional ApplicationId? id);

    long addAppInfoEventListener(ApplicationInformationEventCallback eventCallback);

    void removeAppInfoEventListener(long watchId);

  };

  [NoInterfaceObject] interface Application {

    readonly attribute ApplicationInformation appInfo;

    readonly attribute ApplicationContextId contextId;

    void exit();

    void hide();

    RequestedApplicationControl getRequestedAppControl();
  };


  [NoInterfaceObject] interface ApplicationInformation {

    readonly attribute ApplicationId id;

    readonly attribute DOMString name;

    readonly attribute DOMString iconPath;

    readonly attribute DOMString version;

    readonly attribute boolean show;

    readonly attribute DOMString[] categories;

    readonly attribute Date installDate;

    readonly attribute long size;

    readonly attribute PackageId packageId;
  };

  [NoInterfaceObject] interface ApplicationContext {

    readonly attribute ApplicationContextId id;

    readonly attribute ApplicationId appId;

  };

  [Constructor(DOMString key, DOMString[] value)]
  interface ApplicationControlData {

    attribute DOMString key;

    attribute DOMString[] value;

  };

  [Constructor(DOMString operation, optional DOMString? uri,
               optional DOMString? mime, optional DOMString? category,
               optional ApplicationControlData[]? data)]
  interface ApplicationControl {

    attribute DOMString operation;

    attribute DOMString? uri;

    attribute DOMString? mime;

    attribute DOMString? category;

    attribute ApplicationControlData[] data;

  };

  [NoInterfaceObject] interface RequestedApplicationControl {

    readonly attribute ApplicationControl appControl;

    readonly attribute ApplicationId callerAppId;

    void replyResult(optional ApplicationControlData[]? data);

    void replyFailure();

  };


  [NoInterfaceObject] interface ApplicationCertificate {

    readonly attribute DOMString type;

    readonly attribute DOMString value;

  };


  [NoInterfaceObject] interface ApplicationMetaData {

    readonly attribute DOMString key;

    readonly attribute DOMString value;

  };


  [Callback=FunctionOnly, NoInterfaceObject] interface ApplicationInformationArraySuccessCallback {
    void onsuccess(ApplicationInformation[] informationArray);
  };

  [Callback=FunctionOnly, NoInterfaceObject] interface FindAppControlSuccessCallback {
    void onsuccess(ApplicationInformation[] informationArray, ApplicationControl appControl);
  };

  [Callback=FunctionOnly, NoInterfaceObject] interface ApplicationContextArraySuccessCallback {
    void onsuccess(ApplicationContext[] contexts);
  };

  [Callback, NoInterfaceObject] interface ApplicationControlDataArrayReplyCallback {
    void onsuccess(optional ApplicationControlData[]? data);

    void onfailure();
  };

  [Callback, NoInterfaceObject] interface ApplicationInformationEventCallback {
    void oninstalled(ApplicationInformation info);

    void onupdated(ApplicationInformation info);

    void onuninstalled(ApplicationId id);
  };

};
"""

//  println(DocParser.parseLine(DocParser.optionalType, optional))
//  println(DocParser.parseLine(DocParser.packageMethodArgs, optionalArgs))
//  println(DocParser.parseLine(DocParser.packageProperty, "readonly attribute ApplicationManager application;"))
//  println(DocParser.parseLine(DocParser.packageMethodArgs, packageArgs))
//  println(DocParser.parseLine(DocParser.packageMethod, method))
  println(DocParser(s))
}
