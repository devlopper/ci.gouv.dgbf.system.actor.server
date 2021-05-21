package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;

public class AdministrativeUnitLocalitiesNativeReader extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<AdministrativeUnit> {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",AdministrativeUnit.COLUMN_IDENTIFIER
				,AdministrativeUnit.COLUMN_SUB_PREFECTURE_IDENTIFIER,AdministrativeUnit.COLUMN_SUB_PREFECTURE_CODE_NAME
				,AdministrativeUnit.COLUMN_DEPARTMENT_IDENTIFIER,AdministrativeUnit.COLUMN_DEPARTMENT_CODE_NAME
				,AdministrativeUnit.COLUMN_REGION_IDENTIFIER,AdministrativeUnit.COLUMN_REGION_CODE_NAME);
		arguments.getTuple(Boolean.TRUE).add(AdministrativeUnit.VIEW_NAME+" t");
		arguments.getPredicate(Boolean.TRUE).add("t.identifiant IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(AdministrativeUnit administrativeUnit, Object[] array) {
		int i = 1;
		administrativeUnit.setSubPrefecture(Locality.instantiateFromIdentifierCodeName(Locality.class,(String)array[i++],(String)array[i++]));
		administrativeUnit.setDepartment(Locality.instantiateFromIdentifierCodeName(Locality.class,(String)array[i++],(String)array[i++]));
		administrativeUnit.setRegion(Locality.instantiateFromIdentifierCodeName(Locality.class,(String)array[i++],(String)array[i++]));
	}
	
	@Override
	protected Boolean getIsNativeQuery() {
		return Boolean.TRUE;
	}
	
	@Override
	protected Class<AdministrativeUnit> getEntityClass() {
		return AdministrativeUnit.class;
	}
}