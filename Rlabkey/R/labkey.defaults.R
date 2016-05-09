##
#  Copyright (c) 2016 LabKey Corporation
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
##

.lkdefaults <- new.env(parent=emptyenv());

# Set the credentials used for all http or https requests
# For now, just apiKey. TODO: Add email, password, baseUrl, folderPath, and maybe curlOptions & lkOptions
labkey.setDefaults <- function(apiKey=NULL)
{
    if (!is.null(apiKey))
        .lkdefaults[["apiKey"]] = apiKey;
}

labkey.ifApiKey <- function()
{
    if (exists("labkey.apiKey")) {
        labkey.apiKey;
    } else {
        .lkdefaults[["apiKey"]];
    }
}
