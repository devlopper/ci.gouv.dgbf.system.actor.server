package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.collection.CollectionHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public class RequestScopeFunctionsCodesIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader extends RequestScopeFunctionsCodesReader implements Serializable {

	@Override
	protected void __set__(Request request, Object[] array) {
		super.__set__(request, array);
		setIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolder(request, array);
	}
	
	public static void setIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolder(Request request, Object[] array) {
		if(CollectionHelper.isEmpty(request.getScopeFunctionsCodes()))
			return;
		if(request.getIsCreditManagerHolder() == null) {
			for(String code : request.getScopeFunctionsCodes())
				if(code.startsWith(Function.CODE_CREDIT_MANAGER_HOLDER))
					request.setIsCreditManagerHolder(Boolean.TRUE);
		}
		
		if(request.getIsAuthorizingOfficerHolder() == null) {
			for(String code : request.getScopeFunctionsCodes())
				if(code.startsWith(Function.CODE_AUTHORIZING_OFFICER_HOLDER))
					request.setIsAuthorizingOfficerHolder(Boolean.TRUE);
		}
		
		if(request.getIsFinancialControllerHolder() == null) {
			for(String code : request.getScopeFunctionsCodes())
				if(code.startsWith(Function.CODE_FINANCIAL_CONTROLLER_HOLDER))
					request.setIsFinancialControllerHolder(Boolean.TRUE);
		}
		
		if(request.getIsAccountingHolder() == null) {
			for(String code : request.getScopeFunctionsCodes())
				if(code.startsWith(Function.CODE_ACCOUNTING_HOLDER))
					request.setIsAccountingHolder(Boolean.TRUE);
		}
	}
}