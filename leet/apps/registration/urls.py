""" Registration URLs """

__author__ = "Eric Lee <eric.leek@geniecapital.com.tw>"

from django.conf.urls.defaults import *
from django.contrib.auth.views import login, logout

#===============================================================================
# urlpatterns = patterns('registration.views',
#    url(r'^signup/$', 'signup', name="registration-signup"),
#    url(r'^activate/(?P<activation_code>\w+)/$', 'activate_user', name="activate-user"),
#    url(r'^reset_password/(?P<reset_code>\w+)/$', 'reset_password', name="reset-password"),
#    url(r'^input_username/$', 'input_username', name="regist-input-username"),
# 
# )
#===============================================================================

urlpatterns = patterns('registration.views',
    url(r'^signup/$', 'signup', name="registration-signup"),
    
)


urlpatterns += patterns('',
    
    (r'^accounts/login/$',  login,{'template_name':'login.html',}),
    (r'^accounts/logout/$', logout,{'next_page':'/',}),
)