#include <qfile.h>

#include "SettingsManager.h"



SettingsManager::SettingsManager(QString fileName)
	: m_sFileName(fileName)
{
	
}


SettingsManager::~SettingsManager()
{
}

void SettingsManager::resetSettings()
{
	m_lstApplicationSettings.clear();
}

void SettingsManager::appendSetting(ApplicationSettings appSetting)
{
	m_lstApplicationSettings.push_front(appSetting);
}

QVariant SettingsManager::loadValue(QString value)
{
	QSettings settings(m_sFileName, QSettings::IniFormat);
	QVariant setting = settings.value(value, "");
	return setting;
}

void SettingsManager::writeValue(const QString &key, const QVariant &variant)
{
	QSettings settings(m_sFileName, QSettings::IniFormat);

	settings.setValue(key, variant);
}

void SettingsManager::setMaximized(bool max)
{
	bMainWindowMaximized = max;
}

bool SettingsManager::isMaximized(void)
{
	return bMainWindowMaximized;
}

QList<ApplicationSettings>& SettingsManager::loadApplicationSettings()
{
	QSettings settings(m_sFileName, QSettings::IniFormat);
	QStringList keys = settings.childGroups();

	// main settings
	bMainWindowMaximized = (loadValue("Maximized").toBool());

	m_lstApplicationSettings.clear();
	for (QString key : keys)
	{
        if (key.startsWith("VOIDS", Qt::CaseInsensitive))
		{
			ApplicationSettings appSetting;
            appSetting.qsInstanceName = (loadValue(key + "/InstanceName").toString());
            appSetting.qsConfigurationFile = (loadValue(key + "/ConfigurationFile").toString());
            appSetting.qptWindowPostion.setX(loadValue(key + "/WindowPositionX").toInt());
			appSetting.qptWindowPostion.setY(loadValue(key + "/WindowPositionY").toInt());
            appSetting.iInstanceID = loadValue(key + "/InstanceID").toInt();
            appendSetting(appSetting);
		}
	}
	return m_lstApplicationSettings;
}

void SettingsManager::saveApplicationSettings()
{
	QFile settingsFile(m_sFileName);
	settingsFile.remove();

	for (auto setting : m_lstApplicationSettings)
	{
		QString instance = setting.qsInstanceName;
		instance = instance.replace(" ", "_", Qt::CaseInsensitive);

        writeValue(instance + "/InstanceName", setting.qsInstanceName);
        writeValue(instance + "/ConfigurationFile", setting.qsConfigurationFile);
        writeValue(instance + "/WindowPositionX", setting.qptWindowPostion.x());
		writeValue(instance + "/WindowPositionY", setting.qptWindowPostion.y());
        writeValue(instance + "/InstanceID", setting.iInstanceID);
    }
	// main settings
	writeValue("Maximized", bMainWindowMaximized);
}
