class Rectangle(private var _width:Int, private var _height:Int) {

    def width = _width
    def width_=(width: Int) = {_width = width}

    def height = _height
    def height_=(height: Int) = {_height = height}

    override def hashCode = 41 * (41 + width) + height
    override def equals(other: Any) = other match {
        case that: Rectangle => this.width == that.width && this.height == that.height
        case _ => false
}

}
