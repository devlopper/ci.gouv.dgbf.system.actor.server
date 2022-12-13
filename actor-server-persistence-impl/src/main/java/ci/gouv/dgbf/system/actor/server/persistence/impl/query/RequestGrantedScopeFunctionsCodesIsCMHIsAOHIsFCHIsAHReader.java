package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder.Arguments;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public class RequestGrantedScopeFunctionsCodesIsCMHIsAOHIsFCHIsAHReader extends RequestGrantedScopeFunctionsCodesReader implements Serializable {

	@Override
	protected Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		RequestScopeFunctionsCodesIsCMHIsAOHIsFCHIsAHReader
			.prepareQueryStringBuilderArguments(arguments);
		return arguments;
	}
	
	@Override
	protected void __set__(Request request, Object[] array) {
		super.__set__(request, array);
		RequestScopeFunctionsCodesIsCMHIsAOHIsFCHIsAHReader
			.setIsCreditManagerHolderIsAuthorizingOfficerHolderIsFinancialControllerHolderIsAccountingHolder(request, array);
	}
}