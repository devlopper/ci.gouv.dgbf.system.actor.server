package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;

public interface AdministrativeUnitQuerier extends Querier.CodableAndNamable<AdministrativeUnit> {

	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<AdministrativeUnit> implements AdministrativeUnitQuerier,Serializable {
		
		@Override
		protected Class<AdministrativeUnit> getKlass() {
			return AdministrativeUnit.class;
		}
	}
	
	/**/
	
	static AdministrativeUnitQuerier getInstance() {
		return Helper.getInstance(AdministrativeUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(AdministrativeUnit.class);
	}
}