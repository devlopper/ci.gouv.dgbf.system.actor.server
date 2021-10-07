package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class RequestScopeFunctionsCodesReader extends AbstractRequestReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Request.FIELD_IDENTIFIER).addFromTuple("rsf", FieldHelper.join(RequestScopeFunction.FIELD_SCOPE_FUNCTION,ScopeFunction.FIELD_CODE));
		arguments.getTuple(Boolean.TRUE).addJoins("LEFT JOIN RequestScopeFunction rsf ON rsf.request = t");
		return arguments;
	}
	
	@Override
	protected void __set__(Request request, Object[] array) {
		Integer index = 1;
		request.getScopeFunctionsCodes(Boolean.TRUE).add(getAsString(array, index));
	}
	
	@Override
	protected Boolean isEntityHasOnlyArray(Request request) {
		return Boolean.FALSE;
	}
}