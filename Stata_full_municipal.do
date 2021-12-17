<html>
  <head>
    <meta name="robots" content="noindex,nofollow">
    <title>BIG-IP logout page</title>
    <link rel="stylesheet" type="text/css" HREF="/public/include/css/apm.css">
    <script src="/public/include/js/common.js" language="javascript"></script>
    <script src="/public/include/js/u_plugin.js" language="javascript"></script>

    <script language="javascript">
      function InsertActivexControl(clsid, params)
      {
          var container = document.getElementById("logoutActivexContainer");

          if (navigator.appName == "Microsoft Internet Explorer") {
              var paramsCode = "";
              for (var item in params) {
                  paramsCode += "<param name='" + item + "' value='" + params[item] + "'>";
              }

              container.innerHTML =
                  "<center><object classId=CLSID:" + clsid + " border=0 width=1 height=1>" +
                  paramsCode + "</object></center>";
          }
          else {
              var pluginHost = new UPluginHost("", container);
              var control = {"clsid": clsid, "width": 1, "height": 1};
              pluginHost.hostControl(control, params);
          }
      }

      function OnLoad()
      {
        var isBrowserControl = false;
        try {
            isBrowserControl = ("undefined" != typeof(window.external))
                && ("unknown" != typeof(window.external))
                && ("undefined" != typeof(window.external.WebLogonNotifyUser))
                && ("unknown" != typeof(window.external.WebLogonNotifyUser));
        }
        catch(e) {}

        var display_session = get_cookie("LastMRH_Session");
        if(null != display_session) {
          document.getElementById("sessionDIV").innerHTML = '<BR>The session reference number: &nbsp;' + display_session + '<BR><BR>';
          document.getElementById("sessionDIV").style.visibility = "visible";
        }

        if(isBrowserControl) {
            window.external.WebLogonNotifyUser();
        }
        if(!isBrowserControl) {
            document.getElementById("newSessionDIV").style.visibility = "visible";
        }

        try
        {
            if (get_cookie("F5_PWS") == "1")
            {
                document.cookie = "F5_PWS=0; path=/; expires=Fri, 01-Jan-1970 00:00:01 GMT";
                var pwsClassId = "7E73BE8F-FD87-44EC-8E22-023D5FF960FF";
                InsertActivexControl(pwsClassId, {"command": "exit"} );
            }
        }catch (e) {};
      }
    </script>
  </head>

<body onload="OnLoad();">
<script>
try{

    if(window.external && window.external.WebLogonOnAuthFailure) {
        window.external.WebLogonOnAuthFailure();
    }
}catch(e){};
    try {
        window.external.OnHostClose();
    } catch(e) { }
</script>
<meta http-equiv="X-UA-Compatible" content="IE=Edge">
<meta name="viewport" id="viewport" content="initial-scale=1.0" />
<meta name="robots" context="noindex,nofollow">
<script language="javascript">
//orientation toolbox
function setViewport(){
    document.getElementById("viewport").setAttribute('content', Math.abs(window.orientation) == 90 ? 'width=device-height, initial-scale=1' : 'width=device-width, initial-scale=1.0' );
}
setViewport();
window.onorientationchange = function() {
    setViewport();
}
</script>
<table id="page_header">
  <tr >
    <td id="header_leftcell"><img src='/public/images/my/flogo.png'/></td>
    <td id="header_rightcell"></td>
  </tr>
  <tr>
     <td></td>
     <td id="maxTimeoutTd"><span id="maxTimeoutTag" title=""></span></td>
  </tr>
</table>
<noscript>
<div id="noscript_warning_red">JavaScript is not enabled. Please enable JavaScript in your browser or contact your system administrator for assistance.</div>
<div id="noscript_warning_newsession">To open a new session, please  <A href="/">click here.</A></div>
</noscript>
<table id="main_table" class="logout_page">
<tr>
        <td id="main_table_info_cell">
        <table id="interaction_table">
            <tr>
                <td id="interaction_table_header">Your session could not be established.</td>
            </tr>
            <tr>
                <td class="interaction_table_option_cell"><br><DIV ID="sessionDIV" style='visibility:hidden' align=left></DIV>
                    <font color=red>BIG-IP can not find session information in the request. This can happen because your browser restarted after an add-on was installed. If this occurred, click the link below to continue.  This can also happen because cookies are disabled in your browser. If so, enable cookies in your browser and start a new session.</font><br>                    <br>Thank you for using BIG-IP.<br><br>
                    <div id="newSessionDIV" style='visibility:hidden'>To open a new session, please  <A href='/'>click here.</A></div>                </td>
            </tr>
            <tr>
                <td class="interaction_table_footer"></td>
            </tr>
        </table>
    </td>
    <td id="main_table_image_cell"><img src="/public/images/my/tr.gif"></td></tr>
</table>
<div id="page_footer"><div><table><tr><td style="padding-right: 20px"><a href="http://www.dst.dk/support" target=_blank><FONT SIZE=5>Vejledninger</FONT></a></td><td td style="padding-right: 20px"><a href="https://www.dst.dk/ext/4640886167/0/it/Fjernsupport-Windows--exe"><FONT SIZE=5>Fjernsupport - Windows</FONT></a></td><td td style="padding-right: 20px"><a href="https://www.dst.dk/ext/88537103796/0/it/Fjernsupport-Mac--zip"><FONT SIZE=5>Fjernsupport - Mac</FONT></a></td></tr></table></div></div>
<DIV ID="logoutActivexContainer" class="inspectionHostDIVSmall"></DIV>

</body>
</html>
