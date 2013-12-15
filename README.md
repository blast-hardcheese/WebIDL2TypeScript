WebIDL to TypeScript converter
==============================

Created during the [#TizenLA](https://twitter.com/search?q=%23TizenLA) Hackathon at [#CrossCampusLA](https://twitter.com/search?q=%23CrossCampusLA).
This parser was based on the [W3C Working Draft 27 September 2011](http://www.w3.org/TR/2011/WD-WebIDL-20110927/), in an attempt to parse the WebIDL included in Tizen 2.2.1.

## Sample input WebIDL
(Taken from the Tizen API reference)

    module Tizen {

      enum FilterMatchFlag { "EXACTLY", "FULLSTRING", "CONTAINS", "STARTSWITH", "ENDSWITH", "EXISTS" };

    };

    module Application {

      typedef DOMString ApplicationId;

      typedef DOMString ApplicationContextId;

      [NoInterfaceObject] interface ApplicationManagerObject {

        readonly attribute ApplicationManager application;

      };

      Tizen implements ApplicationManagerObject;

      [NoInterfaceObject] interface ApplicationManager {

        Application getCurrentApplication();

        void launch(ApplicationId id,
                    optional SuccessCallback? successCallback,
                    optional ErrorCallback? errorCallback);

        void getAppsInfo(ApplicationInformationArraySuccessCallback successCallback,
                         optional ErrorCallback? errorCallback);

        ApplicationInformation getAppInfo(optional ApplicationId? id);

        ApplicationCertificate[] getAppCerts(optional ApplicationId? id);

        ApplicationMetaData[] getAppMetaData(optional ApplicationId? id);

      };

      [NoInterfaceObject] interface Application {

        readonly attribute ApplicationInformation appInfo;

        void exit();

        void hide();

      };

    };

## Output TypeScript definition

    declare enum FilterMatchFlag { "EXACTLY", "FULLSTRING", "CONTAINS", "STARTSWITH", "ENDSWITH", "EXISTS" }

    interface ApplicationId extends String {}
    interface ApplicationContextId extends String {}
    interface ApplicationManagerObject {
      application: ApplicationManager;
    }

    interface Tizen extends ApplicationManagerObject {}
    interface ApplicationManager {
      getCurrentApplication(): Application;
      launch(id: ApplicationId, successCallback: SuccessCallback, errorCallback: ErrorCallback): void;
      getAppsInfo(successCallback: ApplicationInformationArraySuccessCallback, errorCallback: ErrorCallback): void;
      getAppInfo(id: ApplicationId): ApplicationInformation;
      getAppCerts(id: ApplicationId): ApplicationCertificate[];
      getAppMetaData(id: ApplicationId): ApplicationMetaData[];
    }

    interface Application {
      appInfo: ApplicationInformation;
      exit(): void;
      hide(): void;
    }

## TODO
 - Re-evaluate the handling of Modules
   - Currently modules are collapsed, but that seems to put Enums in global scope
 - Actually write something non-trivial using a generated TypeScript definition
