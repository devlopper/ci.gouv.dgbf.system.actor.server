package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntityReaderImpl extends EntityReader.AbstractImpl implements Serializable {

	@SuppressWarnings("unchecked")
	@Override
	public <T> T readOne(Class<T> tupleClass, QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null) {
			if(ActorQuerier.QUERY_IDENTIFIER_READ_ONE_WITH_ALL_PRIVILEGES_BY_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return (T) ActorQuerier.getInstance().readOneWithAllPrivilegesByIdentifier((String)arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_IDENTIFIER));
		}
		return super.readOne(tupleClass, arguments);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public <T> Collection<T> readMany(Class<T> tupleClass, QueryExecutorArguments arguments) {
		if(arguments != null) {
			if(PrivilegeQuerier.QUERY_IDENTIFIER_READ_VISIBLE_BY_ACTOR_CODE.equals(arguments.getQuery().getIdentifier()))
				return (Collection<T>) PrivilegeQuerier.getInstance().readVisibleByActorCode((String)arguments.getFilterFieldValue(PrivilegeQuerier.PARAMETER_NAME_ACTOR_CODE));
		}
		return super.readMany(tupleClass, arguments);
	}
}