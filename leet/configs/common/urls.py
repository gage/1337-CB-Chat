""" Leet global URLs """

__author__ = "Eric Lee <eric.lee@geniecapital.com.tw>"


# Uncomment the next two lines to enable the admin:
# from django.contrib import admin
# admin.autodiscover()


from django.conf.urls.defaults import *
from django.conf import settings
from django.contrib.staticfiles.urls import staticfiles_urlpatterns
from django.core.urlresolvers import reverse

from django.contrib import admin
admin.autodiscover()


urlpatterns = patterns('',
    #admin
    (r'^admin/', include(admin.site.urls)),
    # Non-module views
    url(r'^$', 'globals.views.home', name="globals-home"),

    #module views
    (r'^registration/', include('registration.urls')),
    (r'^chats/', include('chats.urls')),

    (r'^chatserver/(?P<url>.*)$', 'httpproxy.views.proxy'),
)

if settings.DEBUG is False and (settings.SITE_DOMAIN == 'localhost' or settings.SITE_DOMAIN == 'localhost.local'):   #if DEBUG is True it will be served automatically
    urlpatterns += patterns('',
            url(r'^static/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.STATIC_ROOT}),



    )

urlpatterns += staticfiles_urlpatterns()

try:
    from urls_lock import *
except ImportError:
    pass

