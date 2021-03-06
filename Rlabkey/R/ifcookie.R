##
# Copyright (c) 2010-2016 LabKey Corporation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
##

ifcookie <- function()
{
if(exists("labkey.sessionCookieName")) {
    Cvalue=1;
    Cname=labkey.sessionCookieName;
    Ccont=labkey.sessionCookieContents
} else {
    labkey.sessionCookieName = NA;
    labkey.sessionCookieContents = NA;
    Cvalue=0;
    Cname=NULL;
    Ccont=NULL
}

return(list(Cvalue=Cvalue,Cname=Cname,Ccont=Ccont))
}
