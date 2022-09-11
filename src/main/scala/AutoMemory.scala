package hv

class AutoMemory:
  protected val hvs: collection.mutable.Set[HyperVector] = 
    collection.mutable.Set.empty

  def add(hv: HyperVector): Boolean = 
    hvs.add(hv)
  def query(nhv: HyperVector): HyperVector = 
    hvs.minBy(hv => nhv differences hv)


object AutoMemory:
  def apply(hvs: HyperVector*): AutoMemory = AutoMemory.from(hvs) 
  
  def from(ito: IterableOnce[HyperVector]): AutoMemory = new AutoMemory:
    override protected val hvs: collection.mutable.Set[HyperVector] = 
      collection.mutable.Set.from(ito) 
