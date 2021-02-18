package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class TransientFieldsProcessorImpl extends org.cyk.utility.persistence.server.TransientFieldsProcessorImpl {

	@Override
	protected void __process__(Class<?> klass,Collection<?> objects, Collection<String> fieldsNames) {
		if(ScopeFunction.class.equals(klass))
			processScopeFunctions(CollectionHelper.cast(ScopeFunction.class, objects),fieldsNames);
		else
			super.__process__(klass,objects, fieldsNames);
	}
	
	/**/
	
	public static void processScopeFunctions(Collection<ScopeFunction> scopeFunctions,Collection<String> fieldsNames) {
		Collection<RequestScopeFunction> requestScopeFunctions = null;
		if(fieldsNames.contains(ScopeFunction.FIELD_REQUESTED) || fieldsNames.contains(ScopeFunction.FIELD_GRANTED))
			requestScopeFunctions = RequestScopeFunctionQuerier.getInstance().readByScopeFunctionsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(scopeFunctions));
		for(String fieldName : fieldsNames) {
			if(ScopeFunction.FIELD_REQUESTED.equals(fieldName)) {
				if(CollectionHelper.isNotEmpty(requestScopeFunctions)) {
					for(ScopeFunction scopeFunction : scopeFunctions) {
						for(RequestScopeFunction requestScopeFunction : requestScopeFunctions) {
							if(scopeFunction.equals(requestScopeFunction.getScopeFunction())) {
								scopeFunction.setRequested(requestScopeFunction.getRequested());
							}
						}					
					}
				}			
			}else if(ScopeFunction.FIELD_GRANTED.equals(fieldName)) {
				if(CollectionHelper.isNotEmpty(requestScopeFunctions)) {
					for(ScopeFunction scopeFunction : scopeFunctions) {
						for(RequestScopeFunction requestScopeFunction : requestScopeFunctions) {
							if(scopeFunction.equals(requestScopeFunction.getScopeFunction())) {
								scopeFunction.setGranted(requestScopeFunction.getGranted());
							}
						}						
					}
				}
			}
		}
	}	
}