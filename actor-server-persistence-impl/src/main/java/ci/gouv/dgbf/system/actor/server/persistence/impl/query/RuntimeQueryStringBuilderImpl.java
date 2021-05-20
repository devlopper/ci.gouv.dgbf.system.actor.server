package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder.Arguments;
import org.cyk.utility.persistence.server.query.string.RuntimeQueryStringBuilder;
import org.cyk.utility.persistence.server.query.string.WhereStringBuilder.Predicate;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class RuntimeQueryStringBuilderImpl extends RuntimeQueryStringBuilder.AbstractImpl implements Serializable {

	@Override
	protected String __build__(QueryExecutorArguments arguments) {
		if(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return AssignmentsQueryStringReadWhereFilterBuilder.getRead(arguments);
		if(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return AssignmentsQueryStringReadWhereFilterBuilder.getCount(arguments);
		return super.__build__(arguments);
	}
	
	@Override
	protected void populatePredicate(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		super.populatePredicate(arguments, builderArguments, predicate, filter);
		if(arguments.getQuery().isIdentifierEqualsDynamic(Locality.class)) {
			addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, LocalityQuerier.PARAMETER_NAME_TYPE);
			addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"t.parent",Locality.FIELD_IDENTIFIER);
		}
	}
}