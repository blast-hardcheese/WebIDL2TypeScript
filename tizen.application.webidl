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
