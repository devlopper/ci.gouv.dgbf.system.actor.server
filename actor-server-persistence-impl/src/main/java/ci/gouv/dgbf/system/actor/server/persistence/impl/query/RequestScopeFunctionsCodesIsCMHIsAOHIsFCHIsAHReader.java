package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder.Arguments;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class RequestScopeFunctionsCodesIsCMHIsAOHIsFCHIsAHReader extends RequestScopeFunctionsCodesReader implements Serializable {

	@Override
	protected void __set__(Request request, Object[] array) {
		super.__set__(request, array);
		setIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolder(request, array);
	}
	
	@Override
	protected Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		prepareQueryStringBuilderArguments(arguments);
		return arguments;
	}
	
	public static void prepareQueryStringBuilderArguments(QueryStringBuilder.Arguments arguments) {
		arguments.getProjection(Boolean.TRUE).addFromTuple("rsf", FieldHelper.join(RequestScopeFunction.FIELD_SCOPE_FUNCTION,ScopeFunction.FIELD_FUNCTION,Function.FIELD_CODE));
	}
	
	public static void setIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolder(Request request, Object[] array) {
		if(array[2] == null)
			return;
		if(request.getIsCreditManagerHolder() == null && Function.CODE_CREDIT_MANAGER_HOLDER.equals(array[2]))
			request.setIsCreditManagerHolder(Boolean.TRUE);		
		else if(request.getIsAuthorizingOfficerHolder() == null && Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(array[2]))
			request.setIsAuthorizingOfficerHolder(Boolean.TRUE);
		else if(request.getIsFinancialControllerHolder() == null && Function.CODE_FINANCIAL_CONTROLLER_HOLDER.equals(array[2]))
			request.setIsFinancialControllerHolder(Boolean.TRUE);
		else if(request.getIsAccountingHolder() == null && Function.CODE_ACCOUNTING_HOLDER.equals(array[2]))
			request.setIsAccountingHolder(Boolean.TRUE);
	}
}