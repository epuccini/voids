#include <QMenu>
#include <QScrollBar>
#include "ImageView.h"

ImageView::ImageView(QWidget *parent, QString path, QMenu* menu) : QWidget(parent)
{
    formImageView.setupUi(this);

    this->imagePath = path;
    this->menuWindows = menu;
    this->simulationImage = nullptr;
    this->scene = nullptr;
    formImageView.graphicsView->setStyleSheet("background-color: white; color: black");
    updateView();
}

ImageView::~ImageView()
{
    if (simulationImage)
        delete simulationImage;
    if (scene)
        delete scene;
}

void ImageView::updateView()
{
    if (simulationImage)
        delete simulationImage;
    simulationImage = new QImage(imagePath);
    if (scene)
        delete scene;
    scene = new QGraphicsScene(this);
    scene->addPixmap(QPixmap::fromImage(*simulationImage).scaledToWidth(820, Qt::SmoothTransformation).scaledToHeight(820, Qt::SmoothTransformation));
    scene->setSceneRect(simulationImage->rect());
    formImageView.graphicsView->setScene(scene);
}

void ImageView::showEvent(QShowEvent *event)
{
}

void ImageView::closeEvent(QCloseEvent *event)
{
    QString imageWindowTitle = "&" + this->windowTitle();

    QList<QAction*> actionList = menuWindows->actions();
    for (QAction* action: actionList)
    {
        QString actionText = action->text();
        if (imageWindowTitle == action->text())
        {
            menuWindows->removeAction(action);
            menuWindows->exec();
        }
    }

}

void ImageView::slotShowImageWindow()
{

}
