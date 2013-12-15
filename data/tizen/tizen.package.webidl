module Package {


    typedef DOMString PackageId;


    [NoInterfaceObject] interface PackageManagerObject {
        readonly attribute PackageManager package;
    };
    Tizen implements PackageManagerObject;


    [NoInterfaceObject] interface PackageManager {


        void install(DOMString path,
                     PackageProgressCallback progressCallback,
                     optional ErrorCallback? errorCallback);


        void uninstall(PackageId id, 
                       PackageProgressCallback progressCallback,
                       optional ErrorCallback? errorCallback);




        void getPackagesInfo(PackageInformationArraySuccessCallback successCallback,
                             optional ErrorCallback? errorCallback);


        PackageInformation getPackageInfo(optional PackageId? id);


        void setPackageInfoEventListener(PackageInformationEventCallback eventCallback);
        

        void unsetPackageInfoEventListener();
    
    };


    [NoInterfaceObject] interface PackageInformation {

        readonly attribute PackageId id;


        readonly attribute DOMString name;


        readonly attribute DOMString iconPath;


        readonly attribute DOMString version;


        readonly attribute long totalSize;


        readonly attribute long dataSize;


        readonly attribute Date lastModified;


        readonly attribute DOMString author;


        readonly attribute DOMString description;


        readonly attribute ApplicationId[] appIds;

    };


    [Callback=FunctionOnly, NoInterfaceObject] interface PackageInformationArraySuccessCallback {


        void onsuccess(PackageInformation[] informationArray);
    };


    [Callback, NoInterfaceObject] interface PackageProgressCallback {


        void onprogress(PackageId id, short progress);


        void oncomplete(PackageId id);
    };


    [Callback, NoInterfaceObject] interface PackageInformationEventCallback {


        void oninstalled(PackageInformation info);


        void onupdated(PackageInformation info);


        void onuninstalled(PackageId id);
    };

};
