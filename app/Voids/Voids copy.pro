#-------------------------------------------------
#
# Project created by QtCreator 2017-12-05T10:59:15
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets network printsupport

TARGET = Voids
TEMPLATE = app
ICON = Voids.icns
RC_FILE = Voids.rc

# The following define makes your compiler emit warnings if you use
# any feature of Qt which as been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

SOURCES += main.cpp\
        mainwindow.cpp \
    	SettingsManager.cpp \
    	LauncherWidget.cpp \
    	Launcher.cpp \
    ConfigurationWidget.cpp \
    LispEngine.cpp \
    LispSettings.cpp \
    ImageView.cpp

HEADERS  += mainwindow.h \
    SettingsManager.h \
    LauncherWidget.h \
    Launcher.h \
    ConfigurationWidget.h \
    LispEngine.h \
    LispSettings.h \
    ImageView.h

FORMS    += mainwindow.ui \
    launcher.ui \
    configuration.ui \
    imageview.ui

macx: LIBS += -L$$PWD/../../../../../../../../../usr/local/Cellar/ecl/16.1.3_1/lib/ -lecl.16.1.3

INCLUDEPATH += $$PWD/../../../../../../../../../usr/local/include
DEPENDPATH += $$PWD/../../../../../../../../../usr/local/include

DISTFILES += \
    Voids.rc

RESOURCES +=

#QMAKE_CXXFLAGS += -std-c++1y
CONFIG += c++14

include(/Users/edward/Qt5.8.0/Src/qttools/src/shared/qtpropertybrowser/qtpropertybrowser.pri)

