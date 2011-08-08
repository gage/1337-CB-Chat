""" CHAT URLs """

__author__ = "Eric Lee <eric.leek@geniecapital.com.tw>"

from django.conf.urls.defaults import *
from django.contrib.auth.views import login, logout

urlpatterns = patterns('chats.views',
    url(r'^chat_home/$', 'chat_home', name="chat-home"),
    
)


urlpatterns += patterns('chats.ajax',
    url(r'^ajax/create_chatroom/$', 'create_chatroom', name="create-chatroom"),                    
                        
                            
                        
                        
)