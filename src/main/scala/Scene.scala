class Scene {

  private var objectList = Vector[Object]()
  private var viewr: Option[Viewer] = None

  def objects = objectList
  def addObject(obj: Object): Unit = objectList = objectList :+ obj

  def addViewer(viewer: Viewer): Unit = this.viewr = Some(viewer)
  def viewer = viewr

}
