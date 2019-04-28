#pragma once

#include <iostream>

#include <QSettings>
#include <QList>
#include <QPoint>

struct ApplicationSettings
{
    QString qsInstanceName;
    int iInstanceID;
    QString qsConfigurationFile;
    QPoint qptWindowPostion;
};

class SettingsManager
{
public:
	SettingsManager(QString fileName);
	~SettingsManager();

	void resetSettings();
	void appendSetting(ApplicationSettings appSetting);
	QVariant loadValue(QString value);
	void writeValue(const QString &key, const QVariant &variant);
	void setMaximized(bool max);
	bool isMaximized(void);

	QList<ApplicationSettings>& loadApplicationSettings();
	void saveApplicationSettings();

private:
	QString m_sFileName;
	bool bMainWindowMaximized;
	QList<ApplicationSettings> m_lstApplicationSettings;
};

