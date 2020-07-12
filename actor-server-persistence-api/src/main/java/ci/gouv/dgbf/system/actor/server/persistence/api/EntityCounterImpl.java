package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;

import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class EntityCounterImpl extends EntityCounter.AbstractImpl implements Serializable {

	@Override
	public <T> Long count(Class<T> tupleClass, QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null) {
			if(FunctionQuerier.QUERY_IDENTIFIER_COUNT_WITH_PROFILES_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier())) {
				arguments.getQuery().setIdentifier(FunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES);
				return super.count(tupleClass, arguments);
			}
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE.equals(arguments.getQuery().getIdentifier())) {
				arguments.getQuery().setIdentifier(ScopeQuerier.QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE);
				return super.count(tupleClass, arguments);
			}
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return ScopeQuerier.getInstance().countWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA_AND_FILTER.equals(arguments.getQuery().getIdentifier()))
				return ScopeQuerier.getInstance().countWhereTypeIsUAAndFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_VISIBLE_SECTIONS_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return ScopeQuerier.getInstance().countVisibleSectionsWhereFilter(arguments);
			if(ScopeQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER_NOT_ASSOCIATED.equals(arguments.getQuery().getIdentifier()))
				return ScopeQuerier.getInstance().countWhereFilterNotAssociated(arguments);
		}
		return super.count(tupleClass, arguments);
	}
}