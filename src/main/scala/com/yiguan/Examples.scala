package com.yiguan

object Examples extends App {

  val gs = Stream.continually(1D)
  val gs2 = gs.scanLeft(1D)((acc, g) => Math.pow(g, 2) + acc)
  val gs2_sqrt = gs2.map(Math.sqrt)
  val lr = gs zip gs2_sqrt map divide.tupled

  def divide: (Double, Double) => Double = _ / _

  lr take 10 foreach println

  import vegas._
  import vegas.data.External._

  /*
  Vegas("A bar chart showing the US population distribution of age groups in 2000.").
    withURL(Population).
    mark(Bar).
    filter("datum.year == 2000").
    encodeY("age", Ordinal, scale=Scale(bandSize=10D)).
    encodeX("people", Quantitative, aggregate=AggOps.Sum, axis=Axis(title="population")).
    show
  Vegas().
    withURL(Population).
    mark(Bar).
    addTransformCalculation("gender", """datum.sex == 2 ? "Female" : "Male"""").
    filter("datum.year == 2000").
    encodeColumn("age", Ord, scale=Scale(padding=4.0), axis=Axis(orient=Orient.Bottom, axisWidth=1.0, offset= -8.0)).
    encodeY("people", Quantitative, aggregate=AggOps.Sum, axis=Axis(title="population", grid=false)).
    encodeX("gender", Nominal, scale=Scale(bandSize = 6.0), hideAxis=false).
    encodeColor("gender", Nominal, scale=Scale(rangeNominals=List("#EA98D2", "#659CCA"))).
    configFacet(cell=CellConfig(strokeWidth = 0)).
    show

  Vegas().
    withURL(Unemployment).
    mark(Area).
    encodeX("date", Temp, timeUnit=TimeUnit.Yearmonth, scale=Scale(nice=Nice.Month),
      axis=Axis(axisWidth=0D, format="%Y", labelAngle=0D)).
    encodeY("count", Quantitative, aggregate=AggOps.Sum).
    configCell(width=300D, height=200D).
    show

  Vegas().
    withURL(Population).
    filter("datum.year == 2000").
    addTransform("gender", "datum.sex == 2 ? \"Female\" : \"Male\"").
    mark(Bar).
    encodeY("people", Quant, AggOps.Sum, axis=Axis(title="population")).
    encodeX("age", Ord, scale=Scale(bandSize= 17)).
    encodeColor("gender", Nominal, scale=Scale(rangeNominals=List("#EA98D2", "#659CCA"))).
    configMark(stacked=StackOffset.Normalize).
    show

  Vegas("A trellis scatterplot showing Horsepower and Miles per gallons, faceted by binned values of Acceleration.").
    withURL(Cars).
    mark(Point).
    encodeX("Horsepower", Quantitative).
    encodeY("Miles_per_Gallon", Quantitative).
    encodeRow("Acceleration", Quantitative, enableBin=true).
    show

  Vegas().
    withURL(Movies).
    mark(Point).
    encodeX("IMDB_Rating", Quantitative, bin=Bin(maxbins=10.0)).
    encodeY("Rotten_Tomatoes_Rating", Quantitative, bin=Bin(maxbins=10.0)).
    encodeSize(aggregate=AggOps.Count, field="*", dataType=Quantitative).
    show
  Vegas().
    withURL(Cars).
    mark(Area).
    encodeX("Acceleration", Quantitative, bin=Bin()).
    encodeY("Horsepower", Quantitative, AggOps.Mean, enableBin=false).
    encodeColor(field="Cylinders", dataType=Nominal).
    show

  Vegas("The Trellis display by Becker et al. helped establish small multiples as a “powerful mechanism for understanding interactions in studies of how a response depends on explanatory variables”. Here we reproduce a trellis of Barley yields from the 1930s, complete with main-effects ordering to facilitate comparison.").
    withURL(Barley).
    mark(Point).
    encodeRow("site", Ordinal).
    encodeX("yield", Quantitative, aggregate = AggOps.Mean).
    encodeY("variety", Ordinal, sortField = Sort("yield", AggOps.Mean), scale = Scale(bandSize = 12.0)).
    encodeColor(field = "year", dataType = Nominal).
    show

  Vegas("A scatterplot with custom star shapes.").
    withURL(Cars).
    mark(Point).
    encodeX("Horsepower", Quant).
    encodeY("Miles_per_Gallon", Quant).
    encodeColor("Cylinders", Nom).
    encodeSize("Weight_in_lbs", Quant).
    configMark(customShape = "M0,0.2L0.2351,0.3236 0.1902,0.0618 0.3804,-0.1236 0.1175,-0.1618 0,-0.4 -0.1175,-0.1618 -0.3804,-0.1236 -0.1902,0.0618 -0.2351,0.3236 0,0.2Z").
    show
  Vegas("A scatterplot showing average horsepower and displacement for cars from different origins.").
    withURL(Cars).
    mark(Point).
    encodeX("Horsepower", Quant, AggOps.Mean).
    encodeY("Displacement", Quant, AggOps.Mean).
    encodeDetail("Origin").
    show

  Vegas("Stock prices of 5 Tech Companies Over Time.").
    withURL(Stocks, formatType = DataFormat.Csv).
    mark(Line).
    encodeX("date", Temp).
    encodeY("price", Quant).
    encodeDetailFields(Field(field="symbol", dataType=Nominal)).
    show

  Vegas("Plot with hard-coded size value").
    withURL(Cars).
    mark(Circle).
    encodeY("Horsepower", Quantitative).
    encodeX("Miles_per_Gallon", Quantitative).
    encodeSize(value=20L).
    show

  Vegas.layered("Plots both mean and IQR as a background layer").
    withURL(Population).
    withLayers(
      Layer().
        mark(Line).
        encodeX("age", Ordinal).
        encodeY("people", aggregate=AggOps.Mean),
      Layer().
        mark(Area).
        encodeX("age", Ordinal).
        encodeY("people", aggregate=AggOps.Q1).
        encodeY2("people", aggregate=AggOps.Q3)
    ).
    show

  Vegas("Plot with legend on the left and a different title ").
    withURL(Cars).
    mark(Point).
    encodeY("Horsepower", Quantitative).
    encodeX("Miles_per_Gallon", Quantitative).
    encodeColor(field="Origin", dataType=Nominal, legend=Legend(orient = "left", title="Place of Origin" )).
    encodeShape(field="Origin", dataType=Nominal, legend=Legend(orient = "left", title="Place of Origin",
      titleColor="red")).
    show

  Vegas("Plot to show Binning options").
    withURL(Movies).
    mark(Bar).
    encodeX("IMDB_Rating", Quantitative, bin = Bin(step = 2.0, maxbins = 3.0)).
    encodeY(field = "*", Quantitative, aggregate = AggOps.Count).
    show

  Vegas("Plot to show Binning options").
    withURL(Movies).
    mark(Bar).
    encodeX("Worldwide_Gross", Quant, bin=Bin(maxbins=20.0), sortOrder=SortOrder.Desc).
    encodeY(field="*", Quant, aggregate=AggOps.Count).
    show

  Vegas("Plot to show usage of encodeText").
    withURL(Cars).
    addTransform("OriginInitial", "datum.Origin[0]").
    mark(Text).
    encodeX("Horsepower", Quantitative).
    encodeY("Miles_per_Gallon", Quantitative).
    encodeColor(field="Origin", dataType= Nominal).
    encodeText(field="OriginInitial", dataType= Nominal).
    show
*/

}
