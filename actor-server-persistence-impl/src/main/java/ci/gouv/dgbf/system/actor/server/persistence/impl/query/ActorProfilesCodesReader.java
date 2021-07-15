package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

public class ActorProfilesCodesReader extends AbstractActorReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t.actor",Actor.FIELD_IDENTIFIER);
		arguments.getProjection(Boolean.TRUE).addFromTuple("p", Profile.FIELD_CODE);
		arguments.getTuple(Boolean.TRUE).add("ActorProfile t").addJoins("LEFT JOIN Profile p ON p = t.profile");
		arguments.getPredicate(Boolean.TRUE).add("t.actor.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected Boolean isEntityHasOnlyArray(Actor entity) {
		return Boolean.FALSE;
	}
	
	@Override
	protected void __set__(Actor actor, Object[] array) {
		Integer index = 1;
		actor.getProfilesCodes(Boolean.TRUE).add(getAsString(array, index++));
	}
}