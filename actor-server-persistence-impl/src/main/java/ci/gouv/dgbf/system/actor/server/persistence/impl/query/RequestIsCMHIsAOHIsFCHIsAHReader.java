package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class RequestIsCMHIsAOHIsFCHIsAHReader extends AbstractRequestReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Request.FIELD_IDENTIFIER);
		arguments.getProjection(Boolean.TRUE).addFromTuple("rsf", FieldHelper.join(RequestScopeFunction.FIELD_SCOPE_FUNCTION,ScopeFunction.FIELD_FUNCTION,Function.FIELD_CODE));
		arguments.getTuple(Boolean.TRUE).addJoins("LEFT JOIN RequestScopeFunction rsf ON rsf.request = t");
		return arguments;
	}
	
	@Override
	protected void __set__(Request request, Object[] array) {
		super.__set__(request, array);
		setIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolder(request, array,1);
	}
	
	public static void setIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolder(Request request, Object[] array,Integer index) {
		if(array[index] == null)
			return;
		if(request.getIsCreditManagerHolder() == null && Function.CODE_CREDIT_MANAGER_HOLDER.equals(array[index]))
			request.setIsCreditManagerHolder(Boolean.TRUE);
		else if(request.getIsAuthorizingOfficerHolder() == null && Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(array[index]))
			request.setIsAuthorizingOfficerHolder(Boolean.TRUE);
		else if(request.getIsFinancialControllerHolder() == null && Function.CODE_FINANCIAL_CONTROLLER_HOLDER.equals(array[index]))
			request.setIsFinancialControllerHolder(Boolean.TRUE);
		else if(request.getIsAccountingHolder() == null && Function.CODE_ACCOUNTING_HOLDER.equals(array[index]))
			request.setIsAccountingHolder(Boolean.TRUE);
	}
}