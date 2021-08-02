package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

public class ActorCodeFirstNameLastNamesReader extends AbstractActorReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Actor.FIELD_IDENTIFIER,Actor.FIELD_CODE);
		arguments.getProjection(Boolean.TRUE).addFromTuple("i", Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES);
		arguments.getTuple(Boolean.TRUE).add("Actor t").addJoins("LEFT JOIN Identity i ON i = t.identity");
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Actor actor, Object[] array) {
		Integer index = 1;
		//Identity Join
		actor.setCode(getAsString(array, index++));
		actor.setFirstName(getAsString(array, index++));
		actor.setLastNames(getAsString(array, index++));
	}
}