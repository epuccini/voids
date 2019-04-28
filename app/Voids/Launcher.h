#pragma once

/*************************************** Includes *********************************/

#include <QDir>
#include <QFile>
#include <QProcess>
#include <QWidget>

#include "LauncherWidget.h"
#include "LispSettings.h"
#include "ImageView.h"

#ifdef ECL_BUILD
#include "LispEngine.h"
#endif

/*************************************** Definition ********************************/

#ifdef Q_OS_MACX
    const QString PICTURE_VIEWER = "open";
    const QString SVG_VIEWER = "/Applications/Firefox.app";
#endif
#ifdef Q_OS_WIN
    const QString PICTURE_VIEWER = "C:\Windows\System32\Windows-Fotoanzeige";
    const QString SVG_VIEWER = "C:\\Programme\\Firefox\\Firefox.exe";
#endif
#ifdef Q_OS_LINUX
    const QString PICTURE_VIEWER = "Nautilus";
    const QString SVG_VIEWER = "Nautilus";
#endif


/*************************************** Interface *********************************/

namespace Ui { class Launcher; }

class Launcher : public LauncherWidget
{
	Q_OBJECT

public:
    Launcher(QWidget *parent = Q_NULLPTR, QMenu* menu = nullptr);
    ~Launcher();

    Ui::FormLauncher& getLauncher(void);

	void showEvent(QShowEvent *event);
	void closeEvent(QCloseEvent *event);
    void setupConfigurationFile(void);
    void startSimulation(void);
    void stopSimulation(void);
    void setupStyleSheets(void);
    void setupLabelText(void);
    QString getErrorImagePath(LispSettings* settings);
    QString getValidationImagePath(LispSettings* settings);
    void openValidationImageExtern(void);
    void openErrorImageExtern(void);
    void hideImageViews(void);

    public slots:
    void slotStartSimulation(void);
    void slotStopSimulation(void);
    void slotPressConfigurationButton(void);
    void slotCloseSimulation(void);
    void slotShowValidationImage(void);
    void slotShowErrorImage(void);
    void processFinished(int, QProcess::ExitStatus);
    void slotSelectImageWindow(QAction* action);

private:
    void showImages(void);

    LispSettings* lispSettings;
    QGraphicsScene *validationScene;
    QGraphicsScene *errorScene;
    Ui::FormLauncher formLauncher;
    QProcess *simulationProcess;
    QProcess * validationImageViewProcess;
    QProcess * errorImageViewProcess;
    QString previousFolder;
    ImageView* validationImageView;
    ImageView* errorImageView;
    QMenu* menuWindows;
    QWidget* mainWindow;

#ifdef ECL_BUILD
    ECL::LispEngine* lispEngine;
#endif
};	
