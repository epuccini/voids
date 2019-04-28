#include <QFileDialog>
#include <QWidget>
#include <QEvent>
#include <QDialog>
#include <QDebug>
#include <QTreeWidget>
#include <QString>
#include <QMessageBox>

#include "LauncherWidget.h"
#include "Launcher.h"

extern QList<Launcher*> qlLauncherList;

LauncherWidget::LauncherWidget(QWidget *parent)
: QWidget(parent)
{
	launchAbort = false;
}


LauncherWidget::~LauncherWidget()
{
	launchAbort = true;
}


bool LauncherWidget::nativeEvent(const QByteArray& eventType, void* message, long * result)
{
	return QWidget::nativeEvent(eventType, message, result);
}

void LauncherWidget::closeEvent(QCloseEvent *event)
{
    this->close();
}

bool LauncherWidget::isAborted(void)
{
    return launchAbort;
}

void LauncherWidget::abort(const char* message)
{
    int resBtn = QMessageBox::question(
        this, "Voids",
        tr(message),
        QMessageBox::Ok, QMessageBox::Ok);
    if (resBtn != QMessageBox::Ok) {
    }
    else {
        launchAbort = true;
    }
}

void LauncherWidget::showEvent(QShowEvent *event)
{
	QWidget::showEvent(event);
	if (event->spontaneous())
		return;

	if (isInitialized)
		return;

	isInitialized = true;
}

void LauncherWidget::setCurrentApplicationSettings(ApplicationSettings& settings)
{
	currentApplicationSettings = settings;
    configurationFile = settings.qsConfigurationFile;
}

ApplicationSettings&  LauncherWidget::getCurrentApplicationSettings(void)
{
    return currentApplicationSettings;
}

QString LauncherWidget::getConfigurationFile(void)
{
    return this->configurationFile;
}

void LauncherWidget::setConfigurationFile(QString file)
{
    this->configurationFile = file;
    currentApplicationSettings.qsConfigurationFile = file;
}
