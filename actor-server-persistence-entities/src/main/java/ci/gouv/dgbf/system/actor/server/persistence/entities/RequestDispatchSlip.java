package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=RequestDispatchSlip.TABLE_NAME)
@AttributeOverrides(value= {
		@AttributeOverride(name = Assignments.FIELD___AUDIT_WHO__,column = @Column(name="AUDIT_ACTEUR"))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_WHAT__,column = @Column(name="AUDIT_ACTION"))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_WHEN__,column = @Column(name="AUDIT_DATE"))
		,@AttributeOverride(name = Assignments.FIELD___AUDIT_FUNCTIONALITY__,column = @Column(name="AUDIT_FONCTIONALITE"))
})
public class RequestDispatchSlip extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableAuditedImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SECTION) @NotNull private Section section;
	@Transient private String sectionAsString;
	@ManyToOne @JoinColumn(name = COLUMN_FUNCTION) @NotNull private Function function;
	@Transient private String functionAsString;
	@Column(name = COLUMN_CREATION_DATE) @NotNull private LocalDateTime creationDate;
	@Transient private String creationDateAsString;
	@Column(name = COLUMN_SENDING_DATE) private LocalDateTime sendingDate;
	@Transient private String sendingDateAsString;
	@Column(name = COLUMN_PROCESSING_DATE) private LocalDateTime processingDate;
	@Transient private String processingDateAsString;
	@Column(name = COLUMN_COMMENT) private String comment;
	
	@Transient private Collection<Request> requests;
	@Transient private Integer numberOfRequests,numberOfRequestsProcessed,numberOfRequestsAccepted,numberOfRequestsRejected,numberOfRequestsNotProcessed;
	@Transient private Boolean isNumberOfRequestsEqualNumberOfRequestsProcessed;
	
	@Transient private String readPageURL;
	
	@Override
	public RequestDispatchSlip setIdentifier(String identifier) {
		return (RequestDispatchSlip) super.setIdentifier(identifier);
	}
	
	@Override
	public RequestDispatchSlip setCode(String code) {
		return (RequestDispatchSlip) super.setCode(code);
	}
	
	@Override
	public RequestDispatchSlip setName(String name) {
		return (RequestDispatchSlip) super.setName(name);
	}

	public static final String FIELD_SECTION = "section";
	public static final String FIELD_SECTION_AS_STRING = "sectionAsString";
	public static final String FIELD_FUNCTION = "function";
	public static final String FIELD_FUNCTION_AS_STRING = "functionAsString";
	public static final String FIELD_CREATION_DATE = "creationDate";
	public static final String FIELD_CREATION_DATE_AS_STRING = "creationDateAsString";
	public static final String FIELD_SENDING_DATE = "sendingDate";
	public static final String FIELD_SENDING_DATE_AS_STRING = "sendingDateAsString";
	public static final String FIELD_PROCESSING_DATE = "processingDate";
	public static final String FIELD_PROCESSING_DATE_AS_STRING = "processingDateAsString";
	public static final String FIELD_COMMENT = "comment";
	public static final String FIELD_NUMBER_OF_REQUESTS = "numberOfRequests";
	public static final String FIELD_IS_NUMBER_OF_REQUESTS_EQUAL_NUMBER_OF_REQUESTS_PROCESSED = "isNumberOfRequestsEqualNumberOfRequestsProcessed";
	public static final String FIELDS_SECTION_FUNCTION = "sectionFunction";
	public static final String FIELDS_SECTION_FUNCTION_AS_STRING = "sectionFunctionAsString";
	public static final String FIELDS_SECTION_AS_CODE_FUNCTION_AS_CODE_ALL_DATES_ALL_NUMBERS_OF_REQUESTS_AS_STRING = "sectionAsCodeFunctionAsCodeAllDatesAllNumbersOfRequestsAsString";
	public static final String FIELDS_ALL_NUMBERS_OF_REQUESTS_AS_STRING = "allNumbersOfRequestsAsString";
	public static final String FIELDS_ALL_DATES_AS_STRING = "allDatesAsString";
	
	public static final String TABLE_NAME = "DM_BORDEREAU";
	
	public static final String COLUMN_SECTION = "SECTION";
	public static final String COLUMN_FUNCTION = "FONCTION";
	public static final String COLUMN_CREATION_DATE = "DATE_CREATION";
	public static final String COLUMN_SENDING_DATE = "DATE_ENVOI";
	public static final String COLUMN_PROCESSING_DATE = "DATE_TRAITEMENT";
	public static final String COLUMN_COMMENT = "COMMENTAIRE";
	
	public static final String LABEL = "Bordereau de demandes";
}