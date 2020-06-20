package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.value.Value;

public interface ActorQuerier extends Querier {

	/**/
	
	static ActorQuerier getInstance() {
		return Helper.getInstance(ActorQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		
	}
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ActorQuerier,Serializable {
		
		
	}
}