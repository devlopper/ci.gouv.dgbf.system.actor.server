package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public class ScopeBudgetSpecializationUnitSectionsReader extends AbstractScopeReaderImpl {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Scope.FIELD_IDENTIFIER).addFromTuple("budgetSpecializationUnit", BudgetSpecializationUnit.FIELD_SECTION_IDENTIFIER);
		arguments.getTuple(Boolean.TRUE).addJoins("JOIN BudgetSpecializationUnit budgetSpecializationUnit ON budgetSpecializationUnit = t");
		return arguments;
	}
	
	@Override
	protected void __set__(Scope scope, Object[] array) {
		int i = 1;
		scope.setSectionAsString(StringUtils.stripToNull(getAsString(array, i)));
	}
}