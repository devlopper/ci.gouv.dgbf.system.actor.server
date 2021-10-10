package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public class RequestGrantedScopeFunctionsCodesIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader extends RequestGrantedScopeFunctionsCodesReader implements Serializable {

	@Override
	protected void __set__(Request request, Object[] array) {
		super.__set__(request, array);
		RequestScopeFunctionsCodesIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolderReader
			.setIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolder(request, array);
	}
}