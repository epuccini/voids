#ifndef IMAGEVIEW_H
#define IMAGEVIEW_H

#include <QWidget>
#include "ui_imageview.h"

class ImageView : public QWidget
{
    Q_OBJECT
public:
    explicit ImageView(QWidget *parent = 0, QString path = "", QMenu* menu = nullptr);
    ~ImageView();

    void showEvent(QShowEvent *event);
    void closeEvent(QCloseEvent *event);
    void updateView();

signals:

public slots:
    void slotShowImageWindow();

private:
    QMenu* menuWindows;
    QString imagePath;
    QGraphicsScene* scene;
    QImage* simulationImage;
    Ui::FormImageView formImageView;
};

#endif // IMAGEVIEW_H
