#pragma once

#include <memory>

#include "qwidget.h"
#include "qdir.h"
#include "SettingsManager.h"
#include "ui_launcher.h"

namespace Ui { class LauncherWidget; }

class LauncherWidget : public QWidget
{
public:
	LauncherWidget(QWidget *parent = Q_NULLPTR);
	~LauncherWidget();

	virtual void closeEvent(QCloseEvent *event);
	virtual void showEvent(QShowEvent *event);
	virtual bool nativeEvent(const QByteArray& eventType, void* message, long * result);

    void abort(const char* message);
    void setCurrentApplicationSettings(ApplicationSettings& settings);
    ApplicationSettings&  getCurrentApplicationSettings(void);
    QString getConfigurationFile(void);
    void setConfigurationFile(QString);
    bool isAborted(void);

protected:
	bool launchAbort;
    QString configurationFile;
	bool isInitialized;
	ApplicationSettings currentApplicationSettings;
};

